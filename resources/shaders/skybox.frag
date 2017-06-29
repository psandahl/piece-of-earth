// Fragment shader for the sky box.
#version 330 core

// The untransformed position from the vertex shader.
in vec3 vPosition;

// The fragment's color.
out vec4 color;

void main()
{
  vec3 sky = vec3(0, 0, 1);
  vec3 horizon = vec3(1, 0, 0);

  color = vec4(mix(horizon, sky, abs(vPosition.y)), 1);
}
