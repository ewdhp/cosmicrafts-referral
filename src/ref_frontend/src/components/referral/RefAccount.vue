<template>
  <div class="account">
    <div><strong>Player ID:</strong> {{ account.playerID }}</div>
    <div><strong>Player Name:</strong> {{ account.playerName }}</div>  
    <div><strong>Referral Tokens:</strong> {{ account.signupTokenSum }}</div>
    <div><strong>Tier Tokens:</strong> {{ account.tierTokenSum }}</div> 
    <div><strong>Multiplier:</strong> {{ account.multiplier }}</div>
    <div><strong>Networth:</strong> {{ account.netWorth }}</div>
  </div>
</template>

<script>
export default {
  name: "Account",
  props: {
    account: Object
  }
};
</script>

<style scoped>
.account {
  background-color: #ffffff;
  padding: 1rem;
  margin-bottom: 1rem;
  border-radius: 8px;
  border: 1px solid #dee2e6;
}
.account div {
  margin-bottom: 0.5rem;
}
</style>
