// Vertex shader for terrain.
#version 330 core

// Attributes for each vertex.
layout (location = 0) in vec3 position;
layout (location = 1) in vec3 normal;
layout (location = 2) in vec2 texCoord;
layout (location = 3) in vec4 color;

// MVP matrix; for the complete transformation of a vertex.
uniform mat4 mvp;

// MV matrix; for the transformation of normals and positions to view space.
uniform mat4 mv;

// Interpolated vertex attributes.
out vec3 vPosition;
out vec3 vNormal;
out vec2 vTexCoord;
out vec4 vColor;

void main()
{
  // Transform position to view space for lightning calculations in the
  // fragment shader.
  vPosition = (mv * vec4(position, 1)).xyz;

  // Transform normal to view space for lightning calculations in the
  // fragment shader.
  vNormal = (mv * vec4(position, 0)).xyz;

  // Texture coordinates for fragment calculation. Flipped on the t dimension.
  vTexCoord = vec2(texCoord.s, 1 - texCoord.t);

  // The color. Just passed on.
  vColor = color;

  // Transformed vertex position for the GL pipeline.
  gl_Position = mvp * vec4(position, 1);
}
