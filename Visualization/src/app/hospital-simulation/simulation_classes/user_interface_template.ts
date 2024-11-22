import * as THREE from "three";
import { GUI } from "three/addons/libs/lil-gui.module.min.js";
import Lights from "./lights_template";


interface TimeSettings {
    hours: number;
}

export default class UserInterface {
    gui: GUI;
    timeSettings: TimeSettings;

    constructor(scene: THREE.Scene, renderer: THREE.WebGLRenderer, document : any) {
        // Helper function to update object color
        const colorCallback = (object: THREE.Light, color: string): void => {
            object.color.set(color);
        };

        // Helper function to update materials for shadow changes
        const shadowsCallback = (enabled: boolean): void => {
            scene.traverseVisible((child: THREE.Object3D) => {
                const material = (child as any).material;
                if (material instanceof THREE.Material) {
                    material.needsUpdate = true;
                } else if (Array.isArray(material)) {
                    material.forEach(element => {
                        if (element instanceof THREE.Material) {
                            element.needsUpdate = true;
                        }
                    });
                }
            });
        };

        this.gui = new GUI({ hideable: false, container: document.getElementById('parent') } as any);
        const guiDomElement = this.gui.domElement;
        guiDomElement.style.position = 'absolute';
        guiDomElement.style.right = '-30px';
        guiDomElement.style.top = '5px';

        // Create the lights folder
        const lightsFolder = this.gui.addFolder("Lights");

        // Create the ambient light folder
        const ambientLightFolder = lightsFolder.addFolder("Ambient Light");

        // Create an ambient light instance
        const ambientLight = new THREE.AmbientLight(0xffffff, 0.5); // Default white light with medium intensity
        scene.add(ambientLight); // Assuming `scene` is your Three.js scene

        // Add GUI controls for the ambient light
        const ambientColor = { color: `#${ambientLight.color.getHexString()}` };
        ambientLightFolder.addColor(ambientColor, "color").onChange(color => colorCallback(ambientLight, color));
        ambientLightFolder.add(ambientLight, "intensity", 0.0, 1.0, 0.01);

        // Create the directional light folder
        const directionalLightFolder = lightsFolder.addFolder("Directional Light");

        // Create a directional light instance
        const directionalLight = new THREE.DirectionalLight(0xffffff, 1); // Default white light with full intensity
        directionalLight.position.set(5, 10, 5); // Set initial position
        scene.add(directionalLight); // Add the light to the scene

        // Add a helper to visualize the light's direction (optional)
        const directionalLightHelper = new THREE.DirectionalLightHelper(directionalLight, 5);
        scene.add(directionalLightHelper); // Add the helper to the scene

        // Object for GUI controls
        const directionalLightSettings = {
            color: `#${directionalLight.color.getHexString()}`,
            x: directionalLight.position.x,
            y: directionalLight.position.y,
            z: directionalLight.position.z,
        };

        // Add GUI controls for the directional light
        directionalLightFolder.addColor(directionalLightSettings, "color").onChange(color => {
            directionalLight.color.set(color);
        });
        directionalLightFolder.add(directionalLight, "intensity", 0.0, 2.0, 0.1); // Intensity control
        directionalLightFolder.add(directionalLightSettings, "x", -20, 20, 0.1).onChange(value => {
            directionalLight.position.x = value;
            directionalLightHelper.update(); // Update helper to reflect changes
        });
        directionalLightFolder.add(directionalLightSettings, "y", -20, 20, 0.1).onChange(value => {
            directionalLight.position.y = value;
            directionalLightHelper.update(); // Update helper to reflect changes
        });
        directionalLightFolder.add(directionalLightSettings, "z", -20, 20, 0.1).onChange(value => {
            directionalLight.position.z = value;
            directionalLightHelper.update(); // Update helper to reflect changes
        });


        // // Create point light #1 folder
        // const pointLight1Folder = lightsFolder.addFolder("Point light #1");
        // const pointLight1 = lights.pointLight1;
        // const pointColor1 = { color: `#${new THREE.Color(pointLight1.color).getHexString()}` };
        // pointLight1Folder.addColor(pointColor1, "color").onChange(color => colorCallback(pointLight1, color));
        // pointLight1Folder.add(pointLight1, "intensity", 0.0, 100.0, 1.0);
        // pointLight1Folder.add(pointLight1, "distance", 0.0, 20.0, 0.01);
        // pointLight1Folder.add(pointLight1.position, "x", -10.0, 10.0, 0.01);
        // pointLight1Folder.add(pointLight1.position, "y", 0.0, 20.0, 0.01);
        // pointLight1Folder.add(pointLight1.position, "z", -10.0, 10.0, 0.01);

        // // Create point light #2 folder
        // const pointLight2Folder = lightsFolder.addFolder("Point light #2");
        // const pointLight2 = lights.pointLight2;
        // const pointColor2 = { color: `#${new THREE.Color(pointLight2.color).getHexString()}` };
        // pointLight2Folder.addColor(pointColor2, "color").onChange(color => colorCallback(pointLight2, color));
        // pointLight2Folder.add(pointLight2, "intensity", 0.0, 100.0, 1.0);
        // pointLight2Folder.add(pointLight2, "distance", 0.0, 20.0, 0.01);
        // pointLight2Folder.add(pointLight2.position, "x", -10.0, 10.0, 0.01);
        // pointLight2Folder.add(pointLight2.position, "y", 0.0, 20.0, 0.01);
        // pointLight2Folder.add(pointLight2.position, "z", -10.0, 10.0, 0.01);

        // Create the shadows folder
       
        const shadowsFolder = this.gui.addFolder("Shadows");
        shadowsFolder.add(renderer.shadowMap, "enabled").onChange(enabled => shadowsCallback(enabled));

        // Create the working day time folder
        this.timeSettings = { hours: 0 }; // Starting at 0 (midnight)
        const timeFolder = this.gui.addFolder("Time");
        timeFolder.add(this.timeSettings, "hours", 0, 24, 0.5);
    }

    setVisibility(visible: boolean): void {
        if (visible) {
            this.gui.show();
        } else {
            this.gui.hide();
        }
    }
}
