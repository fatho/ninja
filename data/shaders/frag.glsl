#version 330 core

in vec2 texCoord_frag;
out vec4 color;

uniform vec4 tint_color;
uniform sampler2D tex;

void main() {
    color = texture(tex, texCoord_frag) * tint_color;
}
