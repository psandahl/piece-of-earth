// Fragment shader for the sky dome.
#version 330 core

// The untransformed position from the vertex shader.
in vec3 vPosition;

// Sky gradient uniforms.
uniform vec3 horizon;
uniform vec3 sky;

// The fragment's color.
out vec4 color;

void main()
{
  color = vec4(mix(horizon, sky, abs(vPosition.y)), 1);
}
