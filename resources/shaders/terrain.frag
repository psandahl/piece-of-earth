// Fragment shader for terrain.
#version 330 core

// Ambient light type for the terrain.
struct AmbientLight
{
  vec3 color;
  float strength;
};

// Light emitter type for the terrain.
struct LightEmitter
{
  vec3 position;
  vec3 color;
};

// Interpolated vertex attributes.
in vec3 vPosition;
in vec3 vNormal;
in vec2 vTexCoord;
in vec4 vColor;

// The ground texture for all terrain.
uniform sampler2D groundTexture;

// The V matrix. For transforming the sun light position to view space.
uniform mat4 vMatrix;

// The ambient light.
uniform AmbientLight ambientLight;

// The sun light.
uniform LightEmitter sunLight;

// Mandatory output; the color for the fragment.
out vec4 color;

// Hardcoded fog color.
vec3 fogColor = vec3(0.5, 0.5, 0.5);

vec3 baseColor();
vec3 calcAmbientLight();
vec3 calcSunLight();
float linearFogFactor();

void main()
{
  vec3 fragmentColor = baseColor() * (calcAmbientLight() + calcSunLight());
  vec3 mixedWithFog = mix(fragmentColor, fogColor, linearFogFactor());
  color = vec4(mixedWithFog, 1.0);
}

// Calculate the base color from the color map and the vertex' color. I.e. the
// color before any lightning is applied.
vec3 baseColor()
{
  vec3 textureColor = texture2D(groundTexture, vTexCoord).rgb;
  return mix(textureColor, vColor.rgb, 0.8);
}

// Calculate the ambient light for the fragment.
vec3 calcAmbientLight()
{
  return ambientLight.color * ambientLight.strength;
}

// Calculate the sun (diffuse) light. This lightning is performed in view space.
vec3 calcSunLight()
{
  vec3 normal = normalize(vNormal);

  // The sun light is already in the correct model coordinates. Just transform
  // it to the view space of the terrain.
  vec3 sunSpot = (vMatrix * vec4(sunLight.position, 1)).xyz;
  vec3 direction = normalize(vPosition - sunSpot);
  float diffuse = max(dot(normal, -direction), 0);

  return sunLight.color * diffuse;
}

float linearFogFactor()
{
  // The fog coordinate given the eye position at 0, 0, 0.
  float fogCoordinate = abs(distance(vec3(0), vPosition));

  float fogStart = 0;
  float fogEnd = 1000;
  float fogDistance = fogEnd - fogStart;

  if (fogCoordinate < fogStart)
  {
    return 0.0;
  }
  else if (fogCoordinate > fogEnd)
  {
    return 1.0;
  }
  else
  {
    float relativeCoord = fogCoordinate - fogStart;
    return smoothstep(0, fogDistance, relativeCoord);
  }
}
