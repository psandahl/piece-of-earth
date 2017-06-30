// Fragment shader for the sky box.
#version 330 core

// The untransformed position from the vertex shader.
in vec3 vPosition;

// The fragment's color.
out vec4 color;

void main()
{
  //vec3 sky = vec3(45.0 / 255.0, 36.0 / 255.0, 57.0 / 255.0);
  //vec3 horizon = vec3(156.0 / 255.0, 59.0 / 255.0, 26.0 / 255.0);

  vec4 sky = vec4(0, 5.0 / 255.0, 25.0 / 255.0, 1);
  vec4 horizon = vec4(189.0 / 255.0, 213.0 / 255.0, 213.0 / 255.0, 1);

  color = mix(horizon, sky, abs(vPosition.y));
}
