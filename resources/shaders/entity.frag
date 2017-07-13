// Fragment shader for entity.
#version 330 core

// Ambient light type.
struct AmbientLight
{
  vec3 color;
  float strength;
};

// Light emitter type.
struct LightEmitter
{
  vec3 position;
  vec3 color;
};

// Interpolated vertex attributes.
in vec3 vPosition;
in vec3 vNormal;
in vec2 vTexCoord;

// The V matrix. For transforming the sun light position to view space.
uniform mat4 vMatrix;

// The ambient light.
uniform AmbientLight ambientLight;

// The sun light.
uniform LightEmitter sunLight;

// Mandatory output; the color for the fragment.
out vec4 color;

vec3 baseColor();
vec3 calcAmbientLight();
vec3 calcDiffuseLight();
vec3 calcSpecularLight();
vec3 sunDirection();

void main()
{
  vec3 fragmentColor = baseColor() * (calcAmbientLight() + calcDiffuseLight() + calcSpecularLight());
  color = vec4(fragmentColor, 1.0);
}

// The base color for the fragment.
vec3 baseColor()
{
  return vec3(192.0 / 255.0);
}

// Calculate the ambient light for the fragment.
vec3 calcAmbientLight()
{
  return ambientLight.color * ambientLight.strength;
}

// Calculate the sun (diffuse) light. This lightning is performed in view space.
vec3 calcDiffuseLight()
{
  vec3 normal = normalize(vNormal);
  float diffuse = max(dot(normal, -sunDirection()), 0);

  return sunLight.color * diffuse;
}

// Calculate the sun (specular) light. This lightning is performed in view space.
vec3 calcSpecularLight()
{
  vec3 normal = normalize(vNormal);
  vec3 reflectDir = reflect(sunDirection(), normal);
  vec3 viewDir = normalize(vec3(0) - vPosition);
  float specAngle = dot(viewDir, reflectDir);
  float specular = pow(max(specAngle, 0), 64);

  return specular * 10.0 * sunLight.color;
}

// Calculate the sun's direction.
vec3 sunDirection()
{
  // The sun light is already in the correct model coordinates. Just transform
  // it to the view space of the terrain.
  vec3 sunSpot = (vMatrix * vec4(sunLight.position, 1)).xyz;
  return normalize(vPosition - sunSpot);
}
