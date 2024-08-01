<template>
  <div class="box">
    <div ><strong>Tier {{ Number(tier.id) + 1 }}</strong></div>
    <div ><strong>Title:</strong> {{ tier.title }}</div>
    <div ><strong>Description:</strong> {{ tier.desc }}</div>
    <div ><strong>Status:</strong> {{ tier.status }}</div>
    <div ><strong>Token:</strong> {{ tier.token.title }} ({{ tier.token.amount }})</div>
    <button v-if="tier.id < 3 " @click="claimTier" :disabled="loading">
      <span  v-if="loading">Loading...</span>
      <span  v-else>Claim Tier</span>
    </button>
  </div>
</template>

<script>
export default {
  name: "Tier",
  props: {
    tier: Object
  },
  data() {
    return {
      loading: false
    };
  },
  methods: {
    async claimTier() {
      this.loading = true;
      console.log('Loading state set to true');
      try {
        await this.$emit('claim-tier');
        await new Promise(resolve => setTimeout(resolve, 2200));
        this.loading = false;
      } finally {

        console.log('Loading state set to false');
      }
    }
  }
};
</script>

<style scoped>

button:disabled {
  background-color: #6c757d;
  cursor: not-allowed;
}

.box {
  border: 1px solid #ccc;
}

</style>
