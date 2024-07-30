<template>
  <div class="logo">
    <svg width="200" height="200" viewBox="0 0 200 200" xmlns="http://www.w3.org/2000/svg">
  <circle cx="100" cy="100" r="50" fill="black" />
  <rect x="90" y="60" width="20" height="30" fill="black" />
  <circle cx="90" cy="50" r="5" fill="black" />
  <circle cx="110" cy="50" r="5" fill="black" />
  <rect x="80" y="100" width="40" height="10" fill="black" />
  <rect x="70" y="120" width="20" height="30" fill="black" />
  <rect x="110" y="120" width="20" height="30" fill="black" />
</svg>
    <span class="company-name">Cosmicrafts</span>
  </div>
</template>

<script>
export default {
  name: "Logo",
};
</script>

<style scoped>
.logo {
  display: flex;
  align-items: center;
}
.company-name {
  margin-left: 0.5rem;
  font-size: 1.5rem;
  font-weight: bold;
}
</style>
