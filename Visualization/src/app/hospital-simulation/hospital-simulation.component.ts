import { AfterViewInit, Component, ElementRef, HostListener, ViewChild } from '@angular/core';
import * as THREE from "three";
import layoutData from '../../../public/Loquitas.json'
import { GLTFLoader, OrbitControls } from 'three-stdlib';
import Wall from './simulation_classes/wall_template';
import Sprite from './simulation_classes/sprite_template';
import Camera from './simulation_classes/camera_template';
import Orientation from './simulation_classes/orientation';
import UserInterface from './simulation_classes/user_interface_template';
import Ground from './simulation_classes/ground_template';
import { ScheduleSlot } from '../domain/shedule-slot';
import { RoomService } from '../services/room.service';
import { RoomSchedule } from '../domain/room-schedule';
import { SideBarDoctorComponent } from '../components/staff/doctor/sidebar-doctor/side-bar-doctor.component';

type CameraParameters = {
  view: string;
  multipleViewsViewport: THREE.Vector4;
  target: THREE.Vector3;
  initialOrientation: Orientation;
  orientationMin: Orientation;
  orientationMax: Orientation;
  initialDistance: number;
  distanceMin: number;
  distanceMax: number;
  initialZoom: number;
  zoomMin: number;
  zoomMax: number;
  initialFov: number;
  near: number;
  far: number;
};

@Component({
  selector: 'app-hospital-simulation',
  standalone: true,
  imports: [SideBarDoctorComponent],
  templateUrl: './hospital-simulation.component.html',
  styleUrl: './hospital-simulation.component.css'
})

export class HospitalSimulationComponent implements AfterViewInit {

  @ViewChild('myCanvas') private canvasRef!: ElementRef<HTMLCanvasElement>;

  constructor(private roomService: RoomService) {}

  private get canvas(): HTMLCanvasElement {
    return this.canvasRef.nativeElement;
  }

  roomIDs !: string[];
  roomSprites = new Map<string, Sprite>;
  roomLoadedPatients = new Map<string, boolean>;
  roomSchedule = new Map<string, Map<string, ScheduleSlot[]>>;
  scheduleDays !: string[];
  selectedDay : string = '';
  workDayTime : number = 0;

  private roomPositions = new Map<string, THREE.Vector3>;
  patientModel !: THREE.Group;

  private renderer!: THREE.WebGLRenderer | null;
  private camera!: THREE.PerspectiveCamera | null;
  private controls!: OrbitControls;
  private scene!: THREE.Scene | null;
  private animationFrameId: number = 0;

  view!: HTMLSelectElement;
  projection!: HTMLSelectElement;
  horizontal!: HTMLInputElement;
  vertical!: HTMLInputElement;
  distance!: HTMLInputElement;
  zoom!: HTMLInputElement;
  mousemove!: HTMLElement | null;
  userInterface!: UserInterface  | null;

  fixedViewCamera!: Camera;
  changeCameraDistance!: boolean;
  changeCameraOrientation!: boolean;

  mousePosition!: THREE.Vector2;
  mouse !: THREE.Vector2;
  raycaster !: THREE.Raycaster;
  INTERSECTED !: THREE.Group | null;
  objectsToIntersect: THREE.Group[] = [];

  private tooltipCanvas !: HTMLCanvasElement;
  private tooltipContext !: CanvasRenderingContext2D;
  private tooltipTexture !: THREE.Texture;
  private tooltipSprite !: THREE.Sprite;

  private ground!: THREE.Mesh;
  private wall = new THREE.Group();
  private surgical_bed = new THREE.Group();
  private door = new THREE.Group();
  private layout = new THREE.Group();
  private scale = new THREE.Vector3(1.0, 1.0, 1.0);
  private bedScale = new THREE.Vector3(0.03, 0.03, 0.025);
  private patientScale = new THREE.Vector3(0.005, 0.005, 0.005);
  private doorScale = new THREE.Vector3(0.4, 1, 0.6);


  private createScene(): void {

    this.scene = new THREE.Scene();
    this.scene.background = new THREE.Color(0xa0a0a0);

    this.renderer = new THREE.WebGLRenderer({ canvas: this.canvas });
    this.renderer.setSize(this.canvas.clientWidth, this.canvas.clientHeight);
    this.renderer.setPixelRatio(devicePixelRatio);
    document.body.appendChild(this.renderer.domElement);

    this.camera = new THREE.PerspectiveCamera(45, window.innerWidth / window.innerHeight, 1, 10000);

    this.controls = new OrbitControls(this.camera, this.renderer.domElement); // Use active camera
    this.controls.minDistance = 10; // Minimum zoom-out distance
    this.controls.maxDistance = 100;

    this.controls.enableDamping = true; // Enable smooth transitions
    this.controls.dampingFactor = 0.25; // Adjust as needed
    this.controls.mouseButtons = {
      MIDDLE: THREE.MOUSE.DOLLY, // Middle-click dolly (zoom)
      RIGHT: THREE.MOUSE.ROTATE  // Right-click rotation
    };
    this.controls.zoomSpeed = 0.5;
    this.controls.update();

    // // Position the camera above the.layout, looking down
    this.camera.position.set(20, 30, 30); // Higher Y position to hover over.layout
    this.camera.lookAt(0, 0, 0); // Look down at the center of the scene

    this.mousemove = document.getElementById("mousemove");

    this.setupEventListeners();

    this.userInterface = new UserInterface(this.scene, this.renderer, document, this.scheduleDays);

    this.raycaster = new THREE.Raycaster();
    this.mouse = new THREE.Vector2();

    this.tooltipCanvas = document.createElement('canvas');
    const context = this.tooltipCanvas.getContext('2d');
    if (context === null) {
      throw new Error("Failed to get 2D context for tooltipCanvas");
    }
    this.tooltipContext = context;

    this.tooltipCanvas.width = 200;
    this.tooltipCanvas.height = 80;
    this.tooltipContext.font = "Bold 15px Arial";
    this.tooltipTexture = new THREE.Texture(this.tooltipCanvas);
    this.tooltipTexture.needsUpdate = true;

    var spriteMaterial = new THREE.SpriteMaterial({
      map: this.tooltipTexture,
      transparent: true,
    });

    this.tooltipSprite = new THREE.Sprite(spriteMaterial);
    this.tooltipSprite.visible = false;
    this.tooltipSprite.scale.set(1.5, 0.7, 1);
    this.layout.add(this.tooltipSprite);
    this.scene.add(this.tooltipSprite);
  }

  initializeFixedCamera() {
    const cameraData: CameraParameters = {
      view: "fixed",
      multipleViewsViewport: new THREE.Vector4(0.0, 0.0, 1.0, 1.0), // Viewport position and size: fraction of window width and window height
      target: new THREE.Vector3(0.0, 0.0, 0.0), // Target position
      initialOrientation: new Orientation(135.0, -45.0), // Horizontal and vertical orientation and associated limits (expressed in degrees)
      orientationMin: new Orientation(-180.0, -90.0),
      orientationMax: new Orientation(180.0, 0.0),
      initialDistance: 8.0, // Distance to the target and associated limits
      distanceMin: 4.0,
      distanceMax: 16.0,
      initialZoom: 1.0, // Zoom factor and associated limits
      zoomMin: 0.5,
      zoomMax: 2.0,
      initialFov: 45.0, // Field-of-view (expressed in degrees)
      near: 0.01, // Front clipping plane
      far: 100.0 // Back clipping plane
    };

    this.fixedViewCamera = new Camera(cameraData, this.canvas.width, this.canvas.height);

    // Set the active view camera (fixed view)
    this.setActiveViewCamera(this.fixedViewCamera);
  }

  setActiveViewCamera(camera: any) {
    this.fixedViewCamera = camera;
    this.horizontal.min = this.fixedViewCamera.orientationMin.h.toFixed(0);
    this.horizontal.max = this.fixedViewCamera.orientationMax.h.toFixed(0);
    this.vertical.min = this.fixedViewCamera.orientationMin.v.toFixed(0);
    this.vertical.max = this.fixedViewCamera.orientationMax.v.toFixed(0);
    this.distance.min = this.fixedViewCamera.distanceMin.toFixed(1);
    this.distance.max = this.fixedViewCamera.distanceMax.toFixed(1);
    this.zoom.min = this.fixedViewCamera.zoomMin.toFixed(1);
    this.zoom.max = this.fixedViewCamera.zoomMax.toFixed(1);
  }

  raycasterIntersectedObjectTooltip(intersects: any) {
    if (intersects.length > 0) {
        let intersectedObject = intersects[0].object;

        while (intersectedObject.parent && !intersectedObject.parent.userData.isParentGroup) {
            intersectedObject = intersectedObject.parent;
        }

        if (intersectedObject.parent) {
          intersectedObject = intersectedObject.parent;
        }

        if (intersectedObject !== this.INTERSECTED) {
            this.INTERSECTED = intersectedObject;

            if (this.INTERSECTED) {
                let message = this.getAppoitmentInfo(this.INTERSECTED.name);
                if (message) {
                  this.showTooltip(message, intersectedObject.getWorldPosition(new THREE.Vector3()));
                }
            }
        }
    } else {
        if (this.INTERSECTED) {
            this.hideTooltip();
            this.INTERSECTED = null;
        }
    }
}

  showTooltip(message: string, position: THREE.Vector3) {
    this.tooltipContext.clearRect(0, 0, this.tooltipCanvas.width, this.tooltipCanvas.height);
    this.tooltipContext.fillStyle = "rgba(255, 255, 255, 0.9)";
    this.tooltipContext.fillRect(0, 0, 200, 250);
    this.tooltipContext.fillStyle = "black";

    const lines = message.split("\n");

    const lineHeight = 20;
    let yPos = 30;

    for (let line of lines) {
        this.tooltipContext.fillText(line, 10, yPos);
        yPos += lineHeight;
    }

    this.tooltipTexture.needsUpdate = true;

    this.tooltipSprite.position.copy(position);
    this.tooltipSprite.position.y += 4.5;
    this.tooltipSprite.visible = true;
  }

  hideTooltip() {
    this.tooltipSprite.visible = false;
  }

  getAppoitmentInfo(name : string) : string | null{
    var roomID = name.split("-")[1];
    var appointment = this.getRoomAppointment(roomID);

    if (appointment){
      var operation = appointment.name === '' ? "Occupied" : appointment.name;
      console.log(`${operation}\nStart: ${appointment.startTime}\nEnd: ${appointment.endTime}`)
      return `${operation}\nStart: ${appointment.startTime}\nEnd: ${appointment.endTime}`;
    }
    return null;
  }


  getRoomAppointment(roomID: string): ScheduleSlot | null {
    if (this.roomSchedule.has(roomID)) {
        const roomScheduleMap = this.roomSchedule.get(roomID)!;

        if (roomScheduleMap.has(this.selectedDay)) {
            const scheduleSlots = roomScheduleMap.get(this.selectedDay)!;

            for (let schedule of scheduleSlots) {
                const startTime = this.convertTimeToDecimalHours(schedule.startTime);
                const endTime = this.convertTimeToDecimalHours(schedule.endTime);

                if (this.workDayTime >= startTime && this.workDayTime < endTime) {
                    return schedule;
                }
            }
        }
    }

    return null; 
}

  private createModel(modelUrl: string, scale: any): Promise<THREE.Group> {
    let model = new THREE.Group();
    const loader = new GLTFLoader();
    return new Promise((resolve, reject) => {
      loader.load(
        modelUrl,
        (gltf) => {
          model = gltf.scene;
          model.scale.copy(scale);
          resolve(model); 
        },
        (xhr) => {
          console.log((xhr.loaded / xhr.total) * 100 + '% loaded');
        },
        (error) => {
          console.error('An error occurred while loading the model:', error);
          reject(error);
        }
      );
    });
  }

  private async loadLayout(data: any): Promise<void> {
    this.ground = new Ground(data.groundTextureUrl, data.size).object;
    this.ground.position.y = -1.265;
    this.layout.add(this.ground);

    this.wall = new Wall(data.wallTextureUrl).object;

    this.surgical_bed = await this.createModel(data.bedModel, this.bedScale);
    this.patientModel = await this.createModel(data.patientModel, this.patientScale);
    this.door = await this.createModel(data.doorModelUrl, this.doorScale);

    const textureLoader = new THREE.TextureLoader();
    this.door.traverse((child: any) => {
      if (child.isMesh) {
        const material = child.material as THREE.MeshStandardMaterial;
        material.map = textureLoader.load(data.doorTextureUrl);
        material.color.set(0xD7EBFA);
        material.needsUpdate = true;
      }
    });

    // Build the maze
    for (let i = 0; i <= data.size.width; i++) {
      for (let j = 0; j <= data.size.height; j++) {

        // West wall
        if (data.layout[j][i] === 1 || data.layout[j][i] === 3) {
          const wallObject = this.wall.clone();
          wallObject.rotateY(Math.PI / 2);
          wallObject.position.set(i - data.size.width / 2, 0.5, j - data.size.height / 2 - 0.5);
          wallObject.name = "Wall";
          this.layout.add(wallObject);
        }

        // North wall
        if (data.layout[j][i] === 2 || data.layout[j][i] === 3) {
          const wallObject = this.wall.clone();
          wallObject.position.set(i - data.size.width / 2 + 0.5, 0.5, j - data.size.height / 2);
          wallObject.name = "Wall";
          this.layout.add(wallObject);
        }

        //North Surgical bed
        if (data.layout[j][i] === 4) {
          const bedObject = this.surgical_bed.clone();
          bedObject.position.set(i - data.size.width / 2, - 0.8, j - data.size.height / 2 - 0.5);
          bedObject.scale.set(0.03, 0.03, 0.025);
          bedObject.name = "Surgical_Bed";
          bedObject.rotation.y = Math.PI / 2;
          this.layout.add(bedObject);
          this.objectsToIntersect.push(bedObject);
        }

        //North Door
        if (data.layout[j][i] === 5) {
          const doorObject = this.door.clone();
          doorObject.position.set(i - data.size.width / 2, -1.25, j - data.size.height / 2 - 0.4);
          doorObject.position.z -= 0.55;
          doorObject.scale.set(0.7, 1, 0.6);
          doorObject.rotation.y = Math.PI;
          doorObject.name = "North Door";
          this.layout.add(doorObject);
        }

        //South Surgical bed
        if (data.layout[j][i] === 7) {
          const bedObject = this.surgical_bed.clone();
          bedObject.position.set(i - data.size.width / 2, -0.8, j - data.size.height / 2 - 0.5);
          bedObject.scale.set(0.03, 0.03, 0.025);
          bedObject.name = "Surgical_Bed";
          bedObject.rotation.y = - Math.PI / 2;
          this.layout.add(bedObject);
          this.objectsToIntersect.push(bedObject);
        }

        //South Door
        if (data.layout[j][i] === 8) {
          const doorObject = this.door.clone();
          doorObject.position.set(i - data.size.width / 2, -1.25, j - data.size.height / 2 - 0.4);
          doorObject.position.z -= 0.55;
          doorObject.scale.set(0.7, 1, 0.6);
          doorObject.name = "South Door";
          doorObject.rotation.y = Math.PI;
          this.layout.add(doorObject);
        }

      }
    }

  let index = 0;

  for (let i = 0; i <= data.size.width; i++) {
      for (let j = 0; j <= data.size.height; j++) {
          if (data.layout[j][i] === 4 || data.layout[j][i] === 7) {
            if (index < this.roomIDs.length) {  
            
                const position = this.cellToCartesian(data, [j, i]);
                this.roomPositions.set(this.roomIDs[index], position)

                this.roomLoadedPatients.set(this.roomIDs[index], false);

                const sprite = new Sprite("   " + this.roomIDs[index] + "   ", true);
                sprite.object.position.copy(position);
                sprite.object.position.y += 2.5;
                sprite.object.position.x += 0.5;

                this.roomSprites.set(this.roomIDs[index], sprite);
                this.layout.add(sprite.object);

                ++index;
              } else {
                  console.warn(`Not enough room IDs to assign to sprites. Current index: ${index}, roomIDs length: ${this.roomIDs.length}`);
                  break;
              }
          }
      }
  }

    this.layout.scale.set(this.scale.x, this.scale.y, this.scale.z);
    this.scene!.add(this.layout);
  }

  private setupEventListeners() {
  this.renderer!.domElement.addEventListener("wheel", (event) => this.mouseWheel(event));
    document.addEventListener("mousemove", event => {
      this.mouse.x = (event.clientX / window.innerWidth) * 2 - 1;
      this.mouse.y = -(event.clientY / window.innerHeight) * 2 + 1;
    });
  }


  mouseWheel(event: WheelEvent) {
    // Prevent the mouse wheel from scrolling the document's content
    event.preventDefault();
    // Store current mouse position in window coordinates (mouse coordinate system: origin in the top-left corner; window coordinate system: origin in the bottom-left corner)
    this.mousePosition = new THREE.Vector2(event.clientX, window.innerHeight - event.clientY - 1);
    // Select the camera whose view is being pointed
    const cameraView = this.getPointedViewport(this.mousePosition);
    if (cameraView != "none") { // One of the remaining cameras selected
      const cameraIndex = ["fixed", "first-person", "third-person", "top"].indexOf(cameraView);
      this.view.options.selectedIndex = cameraIndex;
      const activeViewCamera = [this.fixedViewCamera][cameraIndex];
      activeViewCamera.updateZoom(-0.001 * event.deltaY);
      this.setActiveViewCamera(activeViewCamera);
    }
  }

  pointerIsOverViewport(pointer: any, viewport: any) {
    return (
      pointer.x >= viewport.x &&
      pointer.x < viewport.x + viewport.width &&
      pointer.y >= viewport.y &&
      pointer.y < viewport.y + viewport.height);
  }

  getPointedViewport(pointer: any) {
    let viewport;
    const cameras = [this.fixedViewCamera];

    for (const camera of cameras) {
        if (!camera || typeof camera.getViewport !== "function") {
            console.warn("Camera or getViewport method is not defined:", camera);
            continue;
        }
        viewport = camera.getViewport();
        if (this.pointerIsOverViewport(pointer, viewport)) {
            return camera.view;
        }
    }
    return "none";
}

  cellToCartesian(data: any, cell: [number, number]): THREE.Vector3 {
    const [row, col] = cell;
    return new THREE.Vector3(
      (col - data.size!.width / 2.0 + 0.5) * this.scale.x,
      0.0,
      (row - data.size!.height / 2.0 + 0.5) * this.scale.z
    );
  }

  cartesianToCell(data: any, position : THREE.Vector3) {
    return [
        Math.floor(position.z / this.scale.z + data.size!.height / 2.0),
        Math.floor(position.x / this.scale.x + data.size!.width / 2.0)
    ];
}


  checkOccupancy() {
    for (let [roomName, roomScheduleMap] of this.roomSchedule) {
        let isOccupied = false;


        if (roomScheduleMap.has(this.selectedDay)) {
            const scheduleSlots = roomScheduleMap.get(this.selectedDay)!;

            for (let schedule of scheduleSlots) {
                const startTime = this.convertTimeToDecimalHours(schedule.startTime);
                const endTime = this.convertTimeToDecimalHours(schedule.endTime);

                if (this.workDayTime >= startTime && this.workDayTime < endTime) {
                    isOccupied = true;
                    break;
                }
            }
        }

        if (isOccupied) {
            if (!this.roomLoadedPatients.get(roomName)) {
                this.loadPatient(roomName);
                this.updateSprite(roomName, false);
            }
        } 
        else if (this.roomLoadedPatients.get(roomName) == true) {
                this.removePatient(roomName);
                this.updateSprite(roomName, true);
        }
    }
  }

  convertTimeToDecimalHours(timeString: string): number {
    const [hours, minutes] = timeString.split(':').map(Number);
    return hours + minutes / 60;
  }

  loadPatient(room: string) {
      const RoomPosition = this.roomPositions.get(room);
      if (!RoomPosition) {
          console.error(`Room position not found for room: ${room}`);
          return;
      }

      const patientGroup = new THREE.Group();


      const cell = this.cartesianToCell(layoutData, RoomPosition);
      if (cell) {
          const [j, i] = cell;
          const cellType = layoutData.layout[j][i];

          patientGroup.name = `Patient-${room}`;
          patientGroup.add(this.patientModel.clone());
          patientGroup.position.copy(RoomPosition);
          patientGroup.position.y -= 0.5;
          patientGroup.position.x -= 0.5;
          patientGroup.scale.set(1.5, 1.5, 1.5);
          patientGroup.userData['isParentGroup'] = true;


          if (cellType === 4) {
            patientGroup.rotation.y = - Math.PI;
            patientGroup.position.z -= 0.7;
          } else if (cellType === 7) {
            patientGroup.rotation.y =  Math.PI * 2;
            patientGroup.position.z -= 1.1;
          }
      } else {
          console.warn(`Could not determine cell for position: ${RoomPosition}`);
      }

      this.objectsToIntersect.push(patientGroup);
      this.layout.add(patientGroup);

      this.roomLoadedPatients.set(room, true);
  }

    removePatient(room: string) {
      const patientName = `Patient-${room}`;
      const patient = this.layout.getObjectByName(patientName);

      if (patient) {
          this.layout.remove(patient);
          this.objectsToIntersect = this.objectsToIntersect.filter((obj) => obj !== patient);
          this.roomLoadedPatients.set(room, false);
      } else {
          console.warn(`No patient found with name: ${patientName}`);
      }
  }

  updateSprite(room: string, occupied: boolean) {
      let sprite = this.roomSprites.get(room);

      if (sprite) {
          sprite.available = occupied;
          sprite.updateColor();
      }else {
        console.warn(`No sprite associated with room id: ${room}`);
      }
  }

 
  startAnimationLoop() {
    const animate = () => {

      this.raycaster.setFromCamera(this.mouse, this.camera!);

      const intersects = this.raycaster.intersectObjects(this.objectsToIntersect);
      this.raycasterIntersectedObjectTooltip(intersects);

      this.workDayTime = this.userInterface!.timeSettings.hours;
      this.selectedDay = this.userInterface!.selectedDay.day;

      this.checkOccupancy();

      this.animationFrameId = requestAnimationFrame(animate);
        this.renderer!.render(this.scene!, this.camera!);
        this.controls.update();
    };

    animate();
  }

  ngOnDestroy(): void {
    if (this.animationFrameId) {
      cancelAnimationFrame(this.animationFrameId);
    }
    this.cleanup();
  }


  cleanup() {
    this.renderer!.dispose();
    this.scene = null;
    this.camera = null;

    const canvas = document.getElementById('myCanvas');
    if (canvas) {
        canvas.remove();
    }

    const guiContainer = this.userInterface!.gui?.domElement;
    if (guiContainer && guiContainer.parentNode) {
        guiContainer.parentNode.removeChild(guiContainer);
    }
    this.userInterface = null;
}


  loadDataFromBackend(): void {
    this.roomService.getRoomsSchedule().subscribe((scheduledBackend: RoomSchedule[]) => {
  
      this.roomIDs = scheduledBackend.map((roomSchedule) => roomSchedule.roomNumber);
      this.roomSchedule = new Map<string, Map<string, ScheduleSlot[]>>();
      this.scheduleDays = [];
  
      for (let room of scheduledBackend) {
        const scheduleMap = new Map<string, ScheduleSlot[]>();
  
        room.schedule.forEach((slot) => {
          if (!scheduleMap.has(slot.startDate.split(" ")[0])) {
            scheduleMap.set(slot.startDate.split(" ")[0], []);
          }
          scheduleMap.get(slot.startDate.split(" ")[0])!.push(slot);
        });
  
        this.roomSchedule.set(room.roomNumber, scheduleMap);
      }
  
      this.roomSchedule.forEach((scheduleMap) => {
        scheduleMap.forEach((slots, date) => {
          if (!this.scheduleDays.includes(date)) {
            this.scheduleDays.push(date.split(" ")[0]);
          }
        });
      });
  
      this.scheduleDays.sort((a, b) => new Date(b).getTime() - new Date(a).getTime());
  
      this.createScene();
      this.loadLayout(layoutData);
      document.body.appendChild(this.renderer!.domElement);
      this.startAnimationLoop();
    }, (error) => {
      console.error("Error loading data from backend:", error);
    });
  }
  
  async ngAfterViewInit(): Promise<void> {
    this.loadDataFromBackend();
  }  

  @HostListener('window:resize', ['$event'])
  onResize(): void {
    this.resize();
  }

  private resize(): void {
    this.renderer!.setSize(this.canvas.clientWidth, this.canvas.clientHeight);
    this.startAnimationLoop();
  }
}
