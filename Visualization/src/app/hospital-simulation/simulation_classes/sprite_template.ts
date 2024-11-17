import * as THREE from "three";

interface SpriteParameters {
    [key: string]: any;
    // You can add specific properties if you have them
}

interface TextSpriteParameters {
    fontsize?: number;
    fontface?: string;
    borderThickness?: number;
    borderColor?: { r: number; g: number; b: number; a: number };
    backgroundColor?: { r: number; g: number; b: number; a: number };
}

export default class Sprite {
    object: THREE.Group;
    text: string;

    constructor(parameters: SpriteParameters = {}) {
        // Check if parameters are defined and have properties
        if (parameters && typeof parameters === 'object') {
            for (const [key, value] of Object.entries(parameters)) {
                (this as any)[key] = value;
            }
        }

        this.object = new THREE.Group();
        this.text = "Room 4"; // Default text

        // Create a text sprite
        let spritey = this.makeTextSprite(this.text, {
            fontsize: 24,
            fontface: "Georgia",
            borderColor: { r: 255, g: 0, b: 0, a: 1.0 },
            backgroundColor: { r: 255, g: 100, b: 100, a: 0.8 }
        });

        // Add the sprite to the object group
        this.object.add(spritey);
    }

    // Function to create a text sprite
    makeTextSprite(message: string, parameters: TextSpriteParameters = {}): THREE.Sprite {
        // Set default values for parameters if they are not provided
        const fontface = parameters.fontface || "Arial";
        const fontsize = parameters.fontsize || 18;
        const borderThickness = parameters.borderThickness || 4;
        const borderColor = parameters.borderColor || { r: 0, g: 0, b: 0, a: 1.0 };
        const backgroundColor = parameters.backgroundColor || { r: 255, g: 255, b: 255, a: 1.0 };

        // Create a canvas element to draw the text and background
        const canvas = document.createElement('canvas');
        const context = canvas.getContext('2d')!;
        context.font = `Bold ${fontsize}px ${fontface}`;

        // Measure the text width
        const metrics = context.measureText(message);
        const textWidth = metrics.width;

        // Draw the background color
        context.fillStyle = `rgba(${backgroundColor.r},${backgroundColor.g},${backgroundColor.b},${backgroundColor.a})`;

        // Draw the border color
        context.strokeStyle = `rgba(${borderColor.r},${borderColor.g},${borderColor.b},${borderColor.a})`;
        context.lineWidth = borderThickness;

        // Draw rounded rectangle with text
        this.roundRect(context, borderThickness / 2, borderThickness / 2, textWidth + borderThickness, fontsize * 1.4 + borderThickness, 6);

        // Set text color and draw text
        context.fillStyle = "rgba(0, 0, 0, 1.0)";
        context.fillText(message, borderThickness, fontsize + borderThickness);

        // Create a texture from the canvas
        const texture = new THREE.Texture(canvas);
        texture.needsUpdate = true;

        // Create sprite material with the texture
        const spriteMaterial = new THREE.SpriteMaterial({ map: texture });
        const sprite = new THREE.Sprite(spriteMaterial);
        sprite.scale.set(2.5, 2.5, 2.5);
        return sprite;
    }

    updateText(text: string, parameters: TextSpriteParameters = {}): void {
        const borderThickness = parameters.borderThickness || 4;
        const fontsize = parameters.fontsize || 18;

        // Measure the text width
        const canvas = document.createElement('canvas');
        const context = canvas.getContext('2d')!;
        const metrics = context.measureText(text);
        const textWidth = metrics.width;

        // Draw rounded rectangle with text
        this.roundRect(context, borderThickness / 2, borderThickness / 2, textWidth + borderThickness, fontsize * 1.4 + borderThickness, 6);

        // Set text color and draw text
        context.fillStyle = "rgba(0, 0, 0, 1.0)";
        context.fillText(text, borderThickness, fontsize + borderThickness);

        this.text = text;
    }

    // Function to draw rounded rectangles
    roundRect(ctx: CanvasRenderingContext2D, x: number, y: number, w: number, h: number, r: number): void {
        ctx.beginPath();
        ctx.moveTo(x + r, y);
        ctx.lineTo(x + w - r, y);
        ctx.quadraticCurveTo(x + w, y, x + w, y + r);
        ctx.lineTo(x + w, y + h - r);
        ctx.quadraticCurveTo(x + w, y + h, x + w - r, y + h);
        ctx.lineTo(x + r, y + h);
        ctx.quadraticCurveTo(x, y + h, x, y + h - r);
        ctx.lineTo(x, y + r);
        ctx.quadraticCurveTo(x, y, x + r, y);
        ctx.closePath();
        ctx.fill();
        ctx.stroke();
    }
}
