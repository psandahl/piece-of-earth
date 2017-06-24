// Vertex shader for terrain.
#version 330 core

// Attributes for each vertex.
layout (location = 0) in vec3 position;
layout (location = 1) in vec3 normal;
layout (location = 2) in vec2 texCoord;
layout (location = 3) in vec4 color;

// MVP matrix.
uniform mat4 mvp;

// Interpolated texture coordinate.
out vec2 vTexCoord;

// Interpolated vertex color.
out vec4 vColor;

void main()
{
  vTexCoord = vec2(texCoord.s, 1 - texCoord.t);
  vColor = color;
  gl_Position = mvp * vec4(position, 1);
}
