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
  imports: [],
  templateUrl: './hospital-simulation.component.html',
  styleUrl: './hospital-simulation.component.css'
})

export class HospitalSimulationComponent implements AfterViewInit {
  @ViewChild('myCanvas') private canvasRef!: ElementRef<HTMLCanvasElement>;

  private get canvas(): HTMLCanvasElement {
    return this.canvasRef.nativeElement;
  }

  private renderer!: THREE.WebGLRenderer;
  private camera!: THREE.PerspectiveCamera;
  private controls!: OrbitControls;
  private scene!: THREE.Scene;
  private frustumSize = 20;
  private animationFrameId: number = 0;


  viewsPanel!: HTMLElement;
  view!: HTMLSelectElement;
  projection!: HTMLSelectElement;
  horizontal!: HTMLInputElement;
  vertical!: HTMLInputElement;
  distance!: HTMLInputElement;
  zoom!: HTMLInputElement;
  reset!: HTMLButtonElement;
  resetAll!: HTMLButtonElement;
  helpPanel!: HTMLElement;
  subwindowsPanel!: HTMLElement;
  multipleViewsCheckBox!: HTMLInputElement;
  userInterfaceCheckBox!: HTMLInputElement;
  helpCheckBox!: HTMLInputElement;
  mousemove!: HTMLElement | null;
  activeElement!: Element | null;
  userInterface!: UserInterface;
  workDayTime!: number;


  fixedViewCamera!: Camera;
  changeCameraDistance!: boolean;
  changeCameraOrientation!: boolean;

  mousePosition!: THREE.Vector2;
  mouse !: THREE.Vector2;
  raycaster !: THREE.Raycaster;
  INTERSECTED !: THREE.Object3D | null;
  objectsToIntersect: THREE.Group[] = [];


  // Tooltip elements
  private tooltipCanvas !: HTMLCanvasElement;
  private tooltipContext !: CanvasRenderingContext2D;
  private tooltipTexture !: THREE.Texture;
  private tooltipSprite !: THREE.Sprite;

  private ground!: THREE.Mesh;
  private wall = new THREE.Group();
  private patient = new THREE.Group();
  private surgical_bed = new THREE.Group();
  private door = new THREE.Group();
  private layout = new THREE.Group();
  private scale = new THREE.Vector3(1.0, 1.0, 1.0);
  private bedScale = new THREE.Vector3(0.03, 0.03, 0.025);
  private patientScale = new THREE.Vector3(0.005, 0.005, 0.005);
  private doorScale = new THREE.Vector3(0.4, 1, 0.6);

  private roomPositions: THREE.Vector3[] = [];


  private getAspectRatio(): number {
    return this.canvas.clientWidth / this.canvas.clientHeight;
  }

  private createScene(): void {

    this.scene = new THREE.Scene();
    this.scene.background = new THREE.Color(0xa0a0a0);

    // Initialize the renderer
    this.renderer = new THREE.WebGLRenderer({ canvas: this.canvas });
    this.renderer.setSize(this.canvas.clientWidth, this.canvas.clientHeight);
    this.renderer.setPixelRatio(devicePixelRatio);
    document.body.appendChild(this.renderer.domElement);


    const aspectRatio = this.getAspectRatio();

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



    const ambientLight = new THREE.AmbientLight(0xffffff, 3);
    this.scene.add(ambientLight);

    this.getElementsFromHtml();

    // this.initializeFixedCamera();

    this.setupEventListeners();

    this.activeElement = document.activeElement;

    // create Work Day Time
    this.workDayTime = 0;

    // this.userInterface = new UserInterface(this.scene, this.renderer, this.lights);
    this.userInterface = new UserInterface(this.scene, this.renderer, document);

    // const canvasElement = document.getElementById('parent');
    // if (canvasElement) {
    //     canvasElement.appendChild(this.userInterface.domElement);
    // } else {
    //     console.error("Canvas element not found!");
    // }

    // const parentDiv = document.getElementById('parent');
    // if (parentDiv != null){
    //       parentDiv.appendChild(this.userInterface.domElement);
    // }

    this.workDayTime = this.userInterface.timeSettings.hours;

    this.raycaster = new THREE.Raycaster();
    this.mouse = new THREE.Vector2();

    // Create tooltip sprite canvas and context
    this.tooltipCanvas = document.createElement('canvas');
    const context = this.tooltipCanvas.getContext('2d');
    if (context === null) {
      throw new Error("Failed to get 2D context for tooltipCanvas");
    }
    this.tooltipContext = context;

    this.tooltipCanvas.width = 256;  // Set width
    this.tooltipCanvas.height = 64;
    this.tooltipContext.font = "Bold 20px Arial";
    this.tooltipTexture = new THREE.Texture(this.tooltipCanvas);
    this.tooltipTexture.needsUpdate = true;

    // Tooltip sprite material
    var spriteMaterial = new THREE.SpriteMaterial({
      map: this.tooltipTexture,
      transparent: true,
    });

    this.tooltipSprite = new THREE.Sprite(spriteMaterial);
    this.tooltipSprite.visible = false; // Start with hidden tooltip
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

  getElementsFromHtml() {
    // Get and configure the panel's <div> elements
    // this.viewsPanel = document.getElementById("views-panel") as HTMLElement;
    // this.view = document.getElementById("view") as HTMLSelectElement;
    // this.projection = document.getElementById("projection") as HTMLSelectElement;
    // this.horizontal = document.getElementById("horizontal") as HTMLInputElement;
    // this.horizontal.step = "1";
    // this.vertical = document.getElementById("vertical") as HTMLInputElement;
    // this.vertical.step = "1";
    // this.distance = document.getElementById("distance") as HTMLInputElement;
    // this.distance.step = "0.1";
    // this.zoom = document.getElementById("zoom") as HTMLInputElement;
    // this.zoom.step = "0.1";
    // this.reset = document.getElementById('reset') as HTMLButtonElement;
    // this.helpPanel = document.getElementById("help-panel") as HTMLElement;
    // this.helpPanel.style.visibility = "hidden";
    this.subwindowsPanel = document.getElementById("subwindows-panel") as HTMLElement;
    // this.userInterfaceCheckBox = document.getElementById("user-interface") as HTMLInputElement;
    // this.userInterfaceCheckBox.checked = true;
    // this.helpCheckBox = document.getElementById("help") as HTMLInputElement;
    // this.helpCheckBox.checked = false;

    this.mousemove = document.getElementById("mousemove");
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
    this.displayPanel();
  }

  raycasterIntersectedObjectTooltip(intersects: any) {

    if (intersects.length > 0 && intersects[0].object.name != "") {
      const intersectedObject = intersects[0].object;
      if (intersectedObject !== this.INTERSECTED) {
        this.INTERSECTED = intersectedObject;
        console.log("intersected: " + this.INTERSECTED?.name)
        this.showTooltip(intersectedObject.name || "Unnamed Object", intersectedObject.getWorldPosition(new THREE.Vector3()));
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
    this.tooltipContext.fillRect(0, 0, 200, 40);
    this.tooltipContext.fillStyle = "black";
    this.tooltipContext.fillText(message, 10, 30);
    this.tooltipTexture.needsUpdate = true;

    this.tooltipSprite.position.copy(position);
    this.tooltipSprite.position.y += 1.5;
    this.tooltipSprite.visible = true;
  }

  hideTooltip() {
    this.tooltipSprite.visible = false;
  }

  displayPanel() {
    this.view.options.selectedIndex = ["fixed"].indexOf(this.fixedViewCamera.view);
    this.projection.options.selectedIndex = ["perspective", "orthographic"].indexOf(this.fixedViewCamera.projection);
    this.horizontal.value = this.fixedViewCamera.orientation.h.toFixed(0);
    this.vertical.value = this.fixedViewCamera.orientation.v.toFixed(0);
    this.distance.value = this.fixedViewCamera.distance.toFixed(1);
    this.zoom.value = this.fixedViewCamera.zoom.toFixed(1);
  }

  private createModel(modelUrl: string, scale: any): Promise<THREE.Group> {
    let model = new THREE.Group(); // Store the loaded model
    const loader = new GLTFLoader();
    return new Promise((resolve, reject) => {
      loader.load(
        modelUrl,
        (gltf) => {
          model = gltf.scene; // Store the loaded model
          model.scale.copy(scale); // Apply scaling
          resolve(model); // Resolve the promise with the loaded model
        },
        (xhr) => {
          console.log((xhr.loaded / xhr.total) * 100 + '% loaded');
        },
        (error) => {
          console.error('An error occurred while loading the model:', error);
          reject(error); // Reject the promise if an error occurs
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
    this.patient = await this.createModel(data.patientModel, this.patientScale);
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
          this.objectsToIntersect.push(wallObject);
        }

        // North wall
        if (data.layout[j][i] === 2 || data.layout[j][i] === 3) {
          const wallObject = this.wall.clone();
          wallObject.position.set(i - data.size.width / 2 + 0.5, 0.5, j - data.size.height / 2);
          wallObject.name = "Wall";
          this.layout.add(wallObject);
          this.objectsToIntersect.push(wallObject);
        }

        //North Surgical bed
        if (data.layout[j][i] === 4) {
          const bedObject = this.surgical_bed.clone();
          bedObject.position.set(i - data.size.width / 2, - 0.8, j - data.size.height / 2 - 0.5);
          bedObject.scale.set(0.03, 0.03, 0.025);
          bedObject.name = "North Surgical Bed";
          bedObject.rotation.y = Math.PI / 2;
          this.layout.add(bedObject);
          this.objectsToIntersect.push(bedObject);
        }

        //North Door
        if (data.layout[j][i] === 5) {
          const doorObject = this.door.clone();
          doorObject.position.set(i - data.size.width / 2, -1.25, j - data.size.height / 2 - 0.5);
          doorObject.position.z -= 0.55;
          doorObject.scale.set(0.7, 1, 0.6);
          doorObject.name = "North Door";
          this.layout.add(doorObject);
          this.objectsToIntersect.push(doorObject);
        }

        //North Surgical bed + Patient
        if (data.layout[j][i] === 6) {
          const bedObject = this.surgical_bed.clone();
          bedObject.position.set(i - data.size.width / 2, -0.8, j - data.size.height / 2 - 0.5);
          bedObject.scale.set(0.03, 0.03, 0.025);
          bedObject.name = "Surgical Bed";
          bedObject.rotation.y = Math.PI / 2;
          this.layout.add(bedObject);
          this.objectsToIntersect.push(bedObject);

          const patientObject = this.patient.clone();
          patientObject.position.set(i - data.size.width / 2, -0.8, j - data.size.height / 2 - 0.5);
          patientObject.position.y += 0.2;
          patientObject.position.z += 0.1 * 2;
          patientObject.scale.set(0.007, 0.005, 0.007);
          patientObject.name = "Patient";
          patientObject.rotation.y = Math.PI;
          this.layout.add(patientObject);
          this.objectsToIntersect.push(patientObject);
        }

        //South Surgical bed
        if (data.layout[j][i] === 7) {
          const bedObject = this.surgical_bed.clone();
          bedObject.position.set(i - data.size.width / 2, -0.8, j - data.size.height / 2 - 0.5);
          bedObject.scale.set(0.03, 0.03, 0.025);
          bedObject.name = "South Surgical Bed";
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
          this.objectsToIntersect.push(doorObject);
        }


        //South Surgical bed + Patient
        if (data.layout[j][i] === 9) {
          const bedObject = this.surgical_bed.clone();
          bedObject.position.set(i - data.size.width / 2, -0.8, j - data.size.height / 2 - 0.5);
          bedObject.scale.set(0.03, 0.03, 0.025);
          bedObject.name = "Surgical Bed";
          bedObject.rotation.y = - Math.PI / 2;
          this.layout.add(bedObject);
          this.objectsToIntersect.push(bedObject);

          const patientObject = this.patient.clone();
          patientObject.position.set(i - data.size.width / 2, -0.8, j - data.size.height / 2 - 0.9);
          patientObject.position.y += 0.2;
          patientObject.position.z += 0.1 * 2;
          patientObject.scale.set(0.007, 0.005, 0.007);
          patientObject.name = "Patient";
          this.layout.add(patientObject);
          this.objectsToIntersect.push(patientObject);
        }
      }
    }

    //Adding Room ID Sprites
    for (let i = 0; i <= data.size.width; i++) {
      for (let j = 0; j <= data.size.height; j++) {
        if (data.layout[j][i] === 4 || data.layout[j][i] === 6 || data.layout[j][i] === 7 || data.layout[j][i] === 9 ) {
          const position = this.cellToCartesian(data, [j, i]);
          this.roomPositions.push(position);

          const sprite = new Sprite();
          sprite.object.position.copy(position);
          sprite.object.position.y += 2.5;
          sprite.object.position.z -= 1.5;
          this.layout.add(sprite.object);
        }
      }
    }

    this.layout.scale.set(this.scale.x, this.scale.y, this.scale.z);
    this.scene.add(this.layout);
  }

  private setupEventListeners() {
  //  window.addEventListener("resize", (event) => this.windowResize(event));
   // this.renderer.domElement.addEventListener("mousedown", (event) => this.mouseDown(event));
    this.renderer.domElement.addEventListener("mousemove", (event) => this.mouseMove(event));
   // this.renderer.domElement.addEventListener("mouseup", (event) => this.mouseUp(event));
    this.renderer.domElement.addEventListener("wheel", (event) => this.mouseWheel(event));
    this.renderer.domElement.addEventListener("contextmenu", (event) => this.contextMenu(event));

    // Register the event handler to be called on select, input number, or input checkbox change
    // this.view.addEventListener("change", event => this.elementChange(event));
    // this.projection.addEventListener("change", event => this.elementChange(event));
    // this.horizontal.addEventListener("change", event => this.elementChange(event));
    // this.vertical.addEventListener("change", event => this.elementChange(event));
    // this.distance.addEventListener("change", event => this.elementChange(event));
    // this.zoom.addEventListener("change", event => this.elementChange(event));
    // this.userInterfaceCheckBox.addEventListener("change", event => this.elementChange(event));
    // this.helpCheckBox.addEventListener("change", event => this.elementChange(event));
    // Register the event handler to be called on input button click
    // this.reset.addEventListener('click', event => this.buttonClick(event));

    document.addEventListener("mousemove", event => {
      this.mouse.x = (event.clientX / window.innerWidth) * 2 - 1;
      this.mouse.y = -(event.clientY / window.innerHeight) * 2 + 1;
    });
  }

  private windowResize(event: any): void {
    this.fixedViewCamera.updateWindowSize(window.innerWidth, window.innerHeight);
  }

  mouseDown(event: MouseEvent) {
    if (event.buttons == 1 || event.buttons == 2) { // Primary or secondary button down
      // Store current mouse position in window coordinates (mouse coordinate system: origin in the top-left corner; window coordinate system: origin in the bottom-left corner)
      this.mousePosition = new THREE.Vector2(event.clientX, window.innerHeight - event.clientY - 1);
      // Select the camera whose view is being pointed
      const cameraView = this.getPointedViewport(this.mousePosition);
      if (cameraView != "none") {
        // One of the remaining cameras selected
        const cameraIndex = ["fixed", "first-person", "third-person", "top"].indexOf(cameraView);
        this.view.options.selectedIndex = cameraIndex;
        this.setActiveViewCamera([this.fixedViewCamera][cameraIndex]);
        if (event.buttons == 1) { // Primary button down
          this.changeCameraDistance = true;
        }
        else { // Secondary button down
          this.changeCameraOrientation = true;
        }
      }
    }
  }

  mouseMove(event: MouseEvent) {
    if (event.buttons == 1 || event.buttons == 2) { // Primary or secondary button down
      if (this.changeCameraDistance || this.changeCameraOrientation) { // Mouse action in progress
        // Compute mouse movement and update mouse position
        const newMousePosition = new THREE.Vector2(event.clientX, window.innerHeight - event.clientY - 1);
        const mouseIncrement = newMousePosition.clone().sub(this.mousePosition);
        this.mousePosition = newMousePosition;
        if (event.buttons == 1) { // Primary button down
          if (this.changeCameraDistance) {
            this.fixedViewCamera.updateDistance(-0.05 * (mouseIncrement.x + mouseIncrement.y));
            this.displayPanel();
          }
        }
        else { // Secondary button down
          if (this.changeCameraOrientation) {
            const newValue = mouseIncrement.multiply(new THREE.Vector2(-0.5, 0.5));
            this.fixedViewCamera.updateOrientation(new Orientation(newValue.x, newValue.y));
            this.displayPanel();
          }
        }
      }
    }
  }

  mouseUp(event: MouseEvent) {
    // Reset mouse move action
    this.changeCameraDistance = false;
    this.changeCameraOrientation = false;
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

  contextMenu(event: any) {
    // Prevent the context menu from appearing when the secondary mouse button is clicked
    event.preventDefault();
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
    // Check if the pointer is over other camera viewports
    let cameras;
    cameras = [this.fixedViewCamera];
    for (const camera of cameras) {
      viewport = camera.getViewport();
      if (this.pointerIsOverViewport(pointer, viewport)) {
        return camera.view;
      }
    }
    // No camera viewport is being pointed
    return "none";
  }


  elementChange(event: any) {
    switch (event.target.id) {
      case "view":
        this.setActiveViewCamera([this.fixedViewCamera][this.view.options.selectedIndex]);
        break;
      case "projection":
        this.fixedViewCamera.setActiveProjection(["perspective", "orthographic"][this.projection.options.selectedIndex]);
        this.displayPanel();
        break;
      case "horizontal":
      case "vertical":
      case "distance":
      case "zoom":
        if (event.target.checkValidity()) {
          switch (event.target.id) {
            case "horizontal":
            case "vertical":
              this.fixedViewCamera.setOrientation(
                new Orientation(Number(this.horizontal.value), Number(this.vertical.value))
              );
              break;
            case "distance":
              this.fixedViewCamera.setDistance(Number(this.distance.value));
              break;
            case "zoom":
              this.fixedViewCamera.setZoom(Number(this.zoom.value));
              break;
          }
        }
        break;
      case "user-interface":
        this.setUserInterfaceVisibility(event.target.checked);
        break;
      case "help":
        this.setHelpVisibility(event.target.checked);
        break;
    }
  }

  setUserInterfaceVisibility(visible: boolean) {
    this.userInterfaceCheckBox.checked = visible;
    this.viewsPanel.style.visibility = visible ? "visible" : "hidden";
    this.subwindowsPanel.style.visibility = visible ? "visible" : "hidden";
    this.userInterface.setVisibility(visible);
  }

  setHelpVisibility(visible: boolean) { // Hidden: false; visible: true
    this.helpCheckBox.checked = visible;
    this.helpPanel.style.visibility = visible ? "visible" : "hidden";
  }


  buttonClick(event: MouseEvent) {
    const target = event.target as HTMLElement | null;

    if (target && target.id) { // Ensure target is not null and has an id
      switch (target.id) {
        case "reset":
          this.fixedViewCamera.initialize();
          break;
      }
      this.displayPanel();
    } else {
      console.warn("Event target is null or has no id.");
    }
  }


  cellToCartesian(data: any, cell: [number, number]): THREE.Vector3 {
    const [row, col] = cell;
    return new THREE.Vector3(
      (col - data.size!.width / 2.0 + 0.5) * this.scale.x,
      0.0,
      (row - data.size!.height / 2.0 + 0.5) * this.scale.z
    );
  }

  startAnimationLoop() {
    const animate = () => {
      // Update raycaster with mouse position
      this.raycaster.setFromCamera(this.mouse, this.camera);

      // Check for intersections
      const intersects = this.raycaster.intersectObjects(this.layout.children);
      this.raycasterIntersectedObjectTooltip(intersects);



      // Loop
      this.animationFrameId = requestAnimationFrame(animate);
        // Render the scene
        this.renderer.render(this.scene, this.camera);
        this.controls.update();
    };

    animate();
  }

  ngOnDestroy(): void {
    // Cleanup the animation loop
    if (this.animationFrameId) {
      cancelAnimationFrame(this.animationFrameId);
    }
  }

  async ngAfterViewInit(): Promise<void> {
    this.createScene();
    this.loadLayout(layoutData);
    document.body.appendChild(this.renderer.domElement);
    this.startAnimationLoop();
  }

  @HostListener('window:resize', ['$event'])
  onResize(): void {
    this.resize();
  }

  private resize(): void {
    const aspectRatio = this.getAspectRatio();

    // this.camera.left = (this.frustumSize * aspectRatio) / -2;
    // this.camera.right = (this.frustumSize * aspectRatio) / 2;
    // this.camera.top = this.frustumSize / 2;
    // this.camera.bottom = this.frustumSize / -2;
    // this.camera.updateProjectionMatrix();

    this.renderer.setSize(this.canvas.clientWidth, this.canvas.clientHeight);
    //this.render();
    this.startAnimationLoop();
  }
}
