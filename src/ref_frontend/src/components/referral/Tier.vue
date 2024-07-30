<template>
  <div class="tier-card">
    <h3>Tier {{ Number(tier.id) + 1 }}</h3>
    <div><strong>Title:</strong> {{ tier.title }}</div>
    <div><strong>Description:</strong> {{ tier.desc }}</div>
    <div><strong>Status:</strong> {{ tier.status }}</div>
    <div><strong>Token:</strong> {{ tier.token.title }} ({{ tier.token.amount }})</div>
    <button @click="claimTier">Claim Tier</button>
  </div>
</template>

<script>
export default {
  name: "Tier",
  props: {
    tier: Object
  },
  methods: {
    claimTier() {
      // Call the Motoko claimTier function
    }
  }
};
</script>

<style scoped>
.tier-card {
  background-color: #ffffff;
  padding: 1rem;
  margin-bottom: 1rem;
  border-radius: 8px;
  border: 1px solid #dee2e6;
}
h3 {
  margin-bottom: 1rem;
}
button {
  margin-top: 1rem;
  padding: 0.5rem 1rem;
  border: none;
  border-radius: 4px;
  background-color: #007bff;
  color: #ffffff;
  cursor: pointer;
}
</style>
