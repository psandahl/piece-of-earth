// Fragment shader for the sky dome.
#version 330 core

// The untransformed position from the vertex shader.
in vec3 vPosition;

// Sky gradient uniforms.
uniform vec3 horizon;
uniform vec3 sky;

// The fragment's color.
out vec4 color;

vec3 fogColor = vec3(0.5, 0.5, 0.5);

void main()
{
  float y = abs(vPosition.y);
  vec3 skyColor = mix(horizon, sky, y);

  // Divide each of the subspheres into two bands. One band which is sky only
  // and one band which is mixing in the fog color.
  if (y > 0.2)
  {
    // Sky colors inly.
    color = vec4(skyColor, 1);
  }
  else
  {
    // Sky colors mixed with fog color.
    color = vec4(mix(fogColor, skyColor, smoothstep(0.0, 0.2, y)), 1);
  }
}
