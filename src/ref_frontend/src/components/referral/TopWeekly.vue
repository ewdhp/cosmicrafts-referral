<template>
  <div class="top-weekly-card">
    <h3>Top Weekly Referral's</h3>
    <div><strong>Position:</strong> {{ position }}</div>
    <div><strong>Prize:</strong> {{ prize }}</div>
  </div>
</template>

<script>
export default {
  name: "TopWeekly",
  props: {
    position: Number,
    prize: Number
  }
};
</script>

<style scoped>
.top-weekly-card {
  background-color: #ffffff;
  padding: 1rem;
  margin-bottom: 1rem;
  border-radius: 8px;
  border: 1px solid #dee2e6;
}
h3 {
  margin-bottom: 1rem;
}
</style>
