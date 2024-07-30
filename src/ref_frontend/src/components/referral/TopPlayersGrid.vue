<template>
  <div class="top-players-grid">
    <table>
      <thead>
        <tr>
          <th>Player Name</th>
          <th>Multiplier</th>
          <th>Net Worth</th>
        </tr>
      </thead>
      <tbody>
        <tr v-for="player in topPlayers" :key="player.playerName">
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
  }
};
</script>

<style scoped>
.top-players-grid {
  background-color: #ffffff;
  padding: 1rem;
  margin-bottom: 1rem;
  border-radius: 8px;
  border: 1px solid #dee2e6;
}
table {
  width: 100%;
  border-collapse: collapse;
}
th, td {
  padding: 0.5rem;
  border: 1px solid #dee2e6;
}
th {
  background-color: #f1f1f1;
}
</style>
