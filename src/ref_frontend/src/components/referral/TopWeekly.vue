<template>

    <div class="box"><strong>Rank</strong></div>
    <div class="box"><strong>Position:</strong> {{ position }}</div>
    <div class="box"><strong>Prize:</strong> {{ prize }}</div>

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
