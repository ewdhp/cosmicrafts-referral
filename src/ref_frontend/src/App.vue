<template>
  <MainLayout>
    <template v-slot:navbar>
      <Logo />
    </template>

    <div v-if="loading">Loading...</div>
    <div v-else-if="error">{{ error }}</div>
    <div v-else>
      <Account v-if="account" :account="account" />
      <TopPlayersGrid v-if="topPlayers.length" :topPlayers="topPlayers" />
      <TopWeekly 
      v-if="topWeeklyPosition !== null && topWeeklyPrize !== null" 
      :position="Number(topWeeklyPosition)" :prize="Number(topWeeklyPrize)" />
      <ShareLink v-if="signupLink" :link="signupLink" />
      <Tier v-if="currentTier" :tier="currentTier" @claim-tier="claimTier" />
    </div>

    <template v-slot:footer>
      Cosmicrafts All rights reserved
    </template>
  </MainLayout>
</template>

<script>
import MainLayout from "./components/MainLayout.vue";
import Logo from "./components/Logo.vue";
import Account from "./components/referral/RefAccount.vue";
import TopPlayersGrid from "./components/referral/TopPlayersGrid.vue";
import TopWeekly from "./components/referral/TopWeekly.vue";
import ShareLink from "./components/referral/ShareLink.vue";
import Tier from "./components/referral/Tier.vue";

import { ref_backend } from 'declarations/ref_backend/index';
import { Principal } from "@dfinity/principal";

export default {
  name: "App",
  components: {
    MainLayout,
    Logo,
    Account,
    TopPlayersGrid,
    TopWeekly,
    ShareLink,
    Tier
  },
  data() {
    return {
      account: null,
      topPlayers: [],
      topWeeklyPosition: null,
      topWeeklyPrize: null,
      signupLink: "",
      currentTier: null,
      loading: true,
      error: null
    };
  },
  async created() {
    await this.fetchReferralView();
  },
  methods: {
    async fetchReferralView() {
      try {
        const principal = Principal.fromText("jfbyc-gfwca-yk5x5-athbt-zxzvl-ajrrc-6gmfn-6yk23-fzzlj-tt2yi-kps");
        const response = await ref_backend.getRefaccountView(principal);

        console.log('Response from backend:', response);

        if (!response || !response.length) {
          throw new Error('No data received from backend');
        }

        const data = response[0];

        this.account = {
          playerID: data.playerID,
          playerName: data.playerName,
          multiplier: data.multiplier,
          netWorth: data.netWorth,
          tierTokenSum: data.tierTokenSum,
          signupTokenSum: data.signupTokenSum
        };
        this.topPlayers = data.topPlayers || [];
        this.topWeeklyPosition = data.topPosition || null;
        this.topWeeklyPrize = data.topTokenAmount ? data.topTokenAmount[0] : null;
        this.signupLink = data.singupLink || "";
        this.currentTier = data.currentTier || null;
      } catch (error) {
        console.error("Failed to fetch referral view data:", error);
        this.error = error.message;
      } finally {
        this.loading = false;
      }
    },
    async claimTier() {
      try {
        // Implement the call to claimTier() in Motoko
        console.log("Claiming tier...");
        // Handle the result of the claimTier call
      } catch (error) {
        console.error("Failed to claim tier:", error);
      }
    }
  }
};
</script>

<style>
body {
  margin: 0;
  font-family: Arial, sans-serif;
}
</style>
