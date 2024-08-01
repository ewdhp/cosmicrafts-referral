<template>
    <div class="box"><strong>Referral Account</strong></div>

      <div class="box">
        <svg xmlns="http://www.w3.org/2000/svg" width="200" height="200" viewBox="0 0 100 100" fill="none">
        <circle cx="50" cy="50" r="48" stroke="#B0B0B0" stroke-width="4" fill="#E0E0E0"/>
        <path d="M50 55c7 0 14-5 14-12s-7-12-14-12-14 5-14 12 7 12 14 12z" fill="#B0B0B0"/>
        <path d="M36 68c0-8 7-14 14-14s14 6 14 14v6H36v-6z" fill="#B0B0B0"/>
        </svg>
      </div> 

      <div class="box">
        <div><strong>Name:</strong> {{ account.playerName }}</div>  
        <div><strong>Signup tokens:</strong> {{ account.signupTokenSum }}</div>
        <div><strong>Tier tokens:</strong> {{ account.tierTokenSum }}</div> 
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
.box {
  border: 1px solid #ccc;
}

</style>
