#version 330 core

in vec3 vertexPos;
in vec2 texCoord_in;
out vec2 texCoord_frag;

uniform mat4 world_matrix;
uniform mat4 projection_matrix;

void main() {
  vec4 pos = vec4(vertexPos, 1.0);
  gl_Position = projection_matrix * world_matrix * pos;
  texCoord_frag = texCoord_in;
}
