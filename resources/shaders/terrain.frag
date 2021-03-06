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

// Material type.
struct Material
{
  int shine;
  float strength;
};

// Fog.
struct Fog
{
  vec3 color;
  float fogStart;
  float fogEnd;
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

// The material properties.
uniform Material material;

// The fog.
uniform Fog fog;

// Mandatory output; the color for the fragment.
out vec4 color;

vec3 baseColor();
vec3 calcAmbientLight();
vec3 calcDiffuseLight();
vec3 calcSpecularLight();
vec3 sunDirection();
float linearFogFactor();

void main()
{
  vec3 fragmentColor = baseColor() * (calcAmbientLight() +
                                      calcDiffuseLight() +
                                      calcSpecularLight());
  vec3 mixedWithFog = mix(fragmentColor, fog.color, linearFogFactor());
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
  float specular = pow(max(specAngle, 0), material.shine);

  return specular * material.strength * sunLight.color;
}

// Calculate the sun's direction.
vec3 sunDirection()
{
  // The sun light is already in the correct model coordinates. Just transform
  // it to the view space of the terrain.
  vec3 sunSpot = (vMatrix * vec4(sunLight.position, 1)).xyz;
  return normalize(vPosition - sunSpot);
}

// Calculate the mix factor for the fog.
float linearFogFactor()
{
  // The fog coordinate given the eye position at 0, 0, 0.
  float fogCoord = abs(distance(vec3(0), vPosition));

  if (fogCoord < fog.fogStart)
  {
    return 0.0;
  }
  else if (fogCoord > fog.fogEnd)
  {
    return 1.0;
  }
  else
  {
    float fogDistance = fog.fogEnd - fog.fogStart;
    float normalizedFogCoord = fogCoord - fog.fogStart;
    return smoothstep(0, fogDistance, normalizedFogCoord);
  }
}
