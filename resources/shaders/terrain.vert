// Vertex shader for terrain.
#version 330 core

layout (location = 0) in vec3 position;
layout (location = 1) in vec3 normal;
layout (location = 2) in vec2 texCoord;
layout (location = 3) in vec4 color;

uniform mat4 mvp;

out vec4 vColor;

void main()
{
  vColor = color;
  gl_Position = mvp * vec4(position, 1);
}
