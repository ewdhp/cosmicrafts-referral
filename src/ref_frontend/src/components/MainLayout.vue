<template>
  <div class="layout">
    <header class="navbar">
      <slot name="navbar"></slot>
    </header>
    <main class="main-content">
      <slot></slot>
    </main>
    <footer class="footer">
      <slot name="footer"></slot>
    </footer>
  </div>
</template>

<script>
export default {
  name: "MainLayout",
};
</script>

<style scoped>
.layout {
  background-color: #f8f9fa;
  display: flex;
  flex-direction: column;
  min-height: 100vh;
}
.navbar {
  background-color: #ffffff;
  padding: 1rem;
  border-bottom: 1px solid #dee2e6;
}
.main-content {
  flex: 1;
  padding: 2rem;
}
.footer {
  background-color: #ffffff;
  padding: 1rem;
  border-top: 1px solid #dee2e6;
  text-align: center;
}
</style>
