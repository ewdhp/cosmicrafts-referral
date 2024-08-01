<template>
  <div class="box">
    <div class="box"><strong>Top 10</strong></div>
    <table >
      <thead >
        <tr>
          <th>Position</th>
          <th>Player Name</th>
          <th>Multiplier</th>
          <th>Net Worth</th>
        </tr>
      </thead>
      <tbody>
        <tr v-for="(player, index) in sortedPlayers" :key="player.playerName">
          <td>{{ index + 1 }}</td>
          <td>{{ player.playerName }}</td>
          <td>{{ player.multiplier }}</td>
          <td>{{ player.netWorth }}</td>
        </tr>
      </tbody>
    </table>
  </div>
</template>

<script>
export default {
  name: "TopPlayersGrid",
  props: {
    topPlayers: Array
  },
  computed: {
    sortedPlayers() {
      return this.topPlayers;
    }
  }
};
</script>

<style scoped>
.top-players-grid {
  padding: 1rem;
  margin-bottom: 1rem;
  border: 1px solid #dee2e6;
}
table {
  width: 100%;

}
th, td {
  padding: 0.5rem;
  border: 1px solid #dee2e6;
}
th {
  background-color: #f1f1f1;
}
.box {
  border: 1px solid #ccc;
}

</style>
