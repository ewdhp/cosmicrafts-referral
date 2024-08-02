<template>
  <div>
    <div v-if="loading">Loading...</div>
    <div v-else-if="error">{{ error }}</div>
    <div v-else>
      <div class="container">
        <Account v-if="account" :account="account" />
      </div>
      
      <div class="container">
        <TopPlayersGrid v-if="topPlayers.length" :topPlayers="topPlayers" />
      </div>

      <div class="container">
        <TopWeekly v-if="topWeeklyPosition !== null && topWeeklyPrize !== null" 
        :position="Number(topWeeklyPosition)" :prize="Number(topWeeklyPrize)" />
      </div>
      
      <div class="container">
        <Tier v-if="currentTier" :key="tierKey" :tier="currentTier" :loading="tierLoading" @claim-tier="claimTier" />
      </div>
      
      <div class="container">
        <ShareLink v-if="signupLink" :link="signupLink" />
      </div>    
    </div>
  </div>
</template>

<script>
import { ref, nextTick } from 'vue';
import Account from "../components/referral/RefAccount.vue";
import TopPlayersGrid from "../components/referral/TopPlayersGrid.vue";
import TopWeekly from "../components/referral/TopWeekly.vue";
import ShareLink from "../components/referral/ShareLink.vue";
import Tier from "../components/referral/Tier.vue";
import { ref_backend } from 'declarations/ref_backend/index';
import { Principal } from "@dfinity/principal";

export default {
  name: "Referrals",
  components: {
    Account,
    TopPlayersGrid,
    TopWeekly,
    ShareLink,
    Tier
  },
  setup() {
    const account = ref(null);
    const topPlayers = ref([]);
    const topWeeklyPosition = ref(null);
    const topWeeklyPrize = ref(null);
    const signupLink = ref("");
    const currentTier = ref(null);
    const loading = ref(true);
    const error = ref(null);
    const tierLoading = ref(false);
    const tierKey = ref(0);

    const fetchReferralView = async () => {
      try {
        const principal = Principal.fromText("darru-6cu4a-6o3vx-zlz2j-5ebz3-j7hbx-3jj2x-qzmad-ecnv2-harwa-dbq");
        const response = await ref_backend.account_view(principal);

        console.log('Response from backend:', response);

        if (!response || !response.length) {
          throw new Error('No data received from backend');
        }

        const data = response[0];

        account.value = {
          playerID: data.playerID,
          playerName: data.playerName,         
          tierTokenSum: data.tierTokenSum,
          signupTokenSum: data.signupTokenSum,
          multiplier: data.multiplier,
          netWorth: data.netWorth
        };
        topPlayers.value = data.topPlayers || [];
        topWeeklyPosition.value = data.topPosition || null;
        topWeeklyPrize.value = data.topTokenAmount ? data.topTokenAmount[0] : null;
        signupLink.value = data.singupLink || "";
        currentTier.value = data.currentTier || null;
      } catch (error) {
        console.error("Failed to fetch referral view data:", error);
        error.value = error.message;
      } finally {
        loading.value = false;
      }
    };

    const claimTier = async () => {
      tierLoading.value = true;
      try {
        const principal = Principal.fromText("darru-6cu4a-6o3vx-zlz2j-5ebz3-j7hbx-3jj2x-qzmad-ecnv2-harwa-dbq");
        const [success, message] = await ref_backend.claim_tier(principal);
        if (success) {
          await fetchReferralView(); // Refresh data
        } else {
          console.error('Claim tier failed:', message);
        }
      } catch (error) {
        console.error("Failed to claim tier:", error);
      } finally {
        await nextTick();
        tierLoading.value = false;
        tierKey.value += 1; 
      }
    };

    return {
      account,
      topPlayers,
      topWeeklyPosition,
      topWeeklyPrize,
      signupLink,
      currentTier,
      loading,
      error,
      tierLoading,
      tierKey,
      fetchReferralView,
      claimTier
    };
  },
  async created() {
    this.loading = true;
    await this.fetchReferralView();
     this.loading = false;
  }
};
</script>

<style>
.container {
  margin: 10px;
  padding: 10px;
  background-color: #ffffff;
  border: 1px solid #dee2e6;
  overflow: hidden;
}
</style>
