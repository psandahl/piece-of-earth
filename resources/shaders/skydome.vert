// Vertex shader for the sky dome.
#version 330 core

// Attributes for each vertex.
layout (location = 0) in vec3 position;

// The VP matrix. As the sky box always will enclose the camera no model
// matrix component is needed.
uniform mat4 vpMatrix;

// Pass the untransformed position to the fragment shader.
out vec3 vPosition;

void main()
{
  vPosition = position;
  gl_Position = vpMatrix * vec4(position, 1);
}
