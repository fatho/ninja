#version 330 core

in vec3 vertexPos;
in vec2 texCoord_in;

out vec2 texCoord;

uniform mat4 model_matrix;
uniform mat4 view_matrix;

void main() {
  vec4 pos = vec4(1.0);
  pos.xyz = vertexPos;
  gl_Position = view_matrix * model_matrix * pos;
  texCoord = texCoord_in;
}
