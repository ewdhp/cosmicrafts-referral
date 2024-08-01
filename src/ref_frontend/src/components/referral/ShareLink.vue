<template>
  <div class="box">
<div class=""><strong>Link</strong></div>
    <div >
       <span >If someone signup you get 5 tokens free</span>
      <button class="share" @click="copyLink"> {{ buttonText }}</button>
    </div>
</div>
</template>

<script>
export default {
  name: "ShareLink",
  props: {
    link: String
  },
  data() {
    return {
      buttonText: "Share"
    };
  },
  methods: {
    async copyLink() {
      try {
        await navigator.clipboard.writeText(this.link);
        this.buttonText = "Copied!";
        setTimeout(() => {
          this.buttonText = "Share";
        }, 2000); // Reset button text after 2 seconds
      } catch (err) {
        console.error("Failed to copy: ", err);
      }
    }
  }
};
</script>

<style scoped>
.share-link-card {
  background-color: #ffffff;
  padding: 1rem;
  margin-bottom: 1rem;
  border-radius: 8px;
  border: 1px solid #dee2e6;
}
.box {

  border: 1px solid #ccc;
}
.share {
  margin-left : 10px;
  border: none;
  border-radius: 4px;
  background-color: #007bff;
  color: #ffffff;
  cursor: pointer;
}

.box {
 border: 1px solid #ccc;
}

</style>
