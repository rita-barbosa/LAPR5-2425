import * as THREE from "three";

// Define the parameters interface
interface AmbientLightParams {
  color: number;
  intensity: number;
}

interface PointLightParams {
  color: number;
  intensity: number;
  distance: number;
  position: THREE.Vector3;
}

interface SpotLightParams {
  color: number;
  intensity: number;
  distance: number;
  angle: number;
  penumbra: number;
  position: THREE.Vector3;
  direction: number;
}

interface LightParameters {
  ambientLight: AmbientLightParams;
  pointLight1: PointLightParams;
  pointLight2: PointLightParams;
  spotLight?: SpotLightParams; // Optional
}


export default class Lights {
  private parameters: LightParameters;
  public object: THREE.Group;
  public ambientLight : THREE.AmbientLight;
  public pointLight1 : THREE.PointLight;
  public pointLight2 : THREE.PointLight;
  public spotLight? : THREE.SpotLight;

  constructor(parameters: LightParameters) {
    this.parameters = parameters;

    // Create a group to hold all lights
    this.object = new THREE.Group();

    // Create and add the ambient light
    this.ambientLight = new THREE.AmbientLight(
      this.parameters.ambientLight.color,
      this.parameters.ambientLight.intensity
    );
    this.object.add(this.ambientLight);

    // Create and add the first point light
    this.pointLight1 = new THREE.PointLight(
      this.parameters.pointLight1.color,
      this.parameters.pointLight1.intensity,
      this.parameters.pointLight1.distance
    );
    this.pointLight1.position.set(
      this.parameters.pointLight1.position.x,
      this.parameters.pointLight1.position.y,
      this.parameters.pointLight1.position.z
    );
    this.pointLight1.castShadow = true;
    this.pointLight1.shadow.mapSize.width = 512;
    this.pointLight1.shadow.mapSize.height = 512;
    this.pointLight1.shadow.camera.near = 5.0;
    this.pointLight1.shadow.camera.far = 15.0;
    this.object.add(this.pointLight1);

    // Create and add the second point light
    this.pointLight2 = new THREE.PointLight(
      this.parameters.pointLight2.color,
      this.parameters.pointLight2.intensity,
      this.parameters.pointLight2.distance
    );
    this.pointLight2.position.set(
      this.parameters.pointLight2.position.x,
      this.parameters.pointLight2.position.y,
      this.parameters.pointLight2.position.z
    );
    this.pointLight2.castShadow = true;
    this.pointLight2.shadow.mapSize.width = 512;
    this.pointLight2.shadow.mapSize.height = 512;
    this.pointLight2.shadow.camera.near = 5.0;
    this.pointLight2.shadow.camera.far = 15.0;
    this.object.add(this.pointLight2);

    if (this.parameters.spotLight) {
        this.spotLight = new THREE.SpotLight(
        this.parameters.spotLight.color,
        this.parameters.spotLight.intensity,
        this.parameters.spotLight.distance,
        this.parameters.spotLight.angle,
        this.parameters.spotLight.penumbra
      );
      this.spotLight.position.set(
        this.parameters.spotLight.position.x,
        this.parameters.spotLight.position.y,
        this.parameters.spotLight.position.z
      );
      this.object.add(this.spotLight);
    }
  }
}
