// Vertex shader for generic entities.
#version 330 core

// Attributes for each vertex.
layout (location = 0) in vec3 position;
layout (location = 1) in vec3 normal;
layout (location = 2) in vec2 texCoord;

// MVP matrix; for the complete transformation of a vertex.
uniform mat4 mvpMatrix;

// MV matrix: for the transformation of normals and positions to view space.
uniform mat4 mvMatrix;

// Interpolated vertex attributes.
out vec3 vPosition;
out vec3 vNormal;
out vec2 vTexCoord;

void main()
{
  // Transform position to view space for lightning calculations in the
  // fragment shader.
  vPosition = (mvMatrix * vec4(position, 1)).xyz;

  // Transform normal to view space for lightning calculations in the
  // fragment shader.
  vNormal = (mvMatrix * vec4(normal, 0)).xyz;

  // Texture coordinated for fragment calculation. Flipped on the t dimension.
  vTexCoord = vec2(texCoord.s, 1 - texCoord.t);

  // Transformed vertex position for the GL pipeline.
  gl_Position = mvpMatrix * vec4(position, 1);
}
