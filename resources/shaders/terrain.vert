// Vertex shader for terrain.
#version 330 core

// Attributes for each vertex.
layout (location = 0) in vec3 position;
layout (location = 1) in vec3 normal;
layout (location = 2) in vec2 texCoord;
layout (location = 3) in vec4 color;

// MVP matrix.
uniform mat4 mvp;

// Interpolated vertex attributes.
out vec3 vPosition;
out vec3 vNormal;
out vec2 vTexCoord;
out vec4 vColor;

void main()
{
  // Transformed vertex position for the GL pipeline.
  gl_Position = mvp * vec4(position, 1);

  // Transformed vertex position for fragment calculations.
  vPosition = gl_Position.xyz;

  // Transformed normal for fragment calculations.
  vNormal = (mvp * vec4(position, 0)).xyz;

  // Texture coordinates for fragment calculation. Flipped on the t dimension.
  vTexCoord = vec2(texCoord.s, 1 - texCoord.t);

  // The color. Just passed on.
  vColor = color;
}
