<template>
  <MainLayout>
    <template v-slot:navbar>
      <Logo />
    </template>

    <div>
      <Referrals />
    </div>

    <template v-slot:footer>
      Cosmicrafts All rights reserved
    </template>
  </MainLayout>
</template>

<script>
import MainLayout from "./components/MainLayout.vue";
import Logo from "./components/Logo.vue";
import Referrals from "./views/Referrals.vue";

export default {
  name: "App",
  components: {
    MainLayout,
    Logo,
    Referrals
  }
};
</script>

<style>
body {
  margin: 0;
  padding: 0;
}
</style>
