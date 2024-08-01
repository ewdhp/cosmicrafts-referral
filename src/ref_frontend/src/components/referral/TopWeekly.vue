<template>
  <div class="box">
    <div class=""><strong>Rank</strong></div>
    <div class=""><strong>Position:</strong> {{ position }}</div>
    <div class=""><strong>Prize:</strong> {{ prize }}</div>
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

.box {
  border: 1px solid #ccc;
}

</style>
