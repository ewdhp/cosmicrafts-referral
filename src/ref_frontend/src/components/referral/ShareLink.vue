<template>
  <div class="share-link-card">
    <h3>Share Link</h3>
    <div>Share and receive 25 tokens free</div>
    <button class="share-button">
      <svg><!-- SVG content here --></svg> Share
    </button>
  </div>
</template>

<script>
export default {
  name: "ShareLink"
};
</script>

<style scoped>
.share-link-card {
  background-color: #ffffff;
  padding: 1rem;
  margin-bottom: 1rem;
  border-radius: 8px;
  border: 1px solid #dee2e6;
}
.share-button {
  display: flex;
  align-items: center;
  margin-top: 1rem;
  padding: 0.5rem 1rem;
  border: none;
  border-radius: 4px;
  background-color: #007bff;
  color: #ffffff;
  cursor: pointer;
}
.share-button svg {
  margin-right: 0.5rem;
}
</style>
