#version 330 core

in vec2 texCoord;
out vec3 color;

uniform vec3 fill_color;
 
void main() {
    color.b = fill_color.b;
    color.rg = texCoord;
}