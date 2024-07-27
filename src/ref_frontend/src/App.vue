<script setup>
import { ref } from 'vue';
import { ref_backend } from 'declarations/ref_backend/index';
import { Principal } from "@dfinity/principal";

let greeting = ref('');

// Function to enroll player
async function enrollPlayer(signupCodeText, principalText) {
  try {
    // Convert principal text to Principal object
    const principal = Principal.fromText("jfbyc-gfwca-yk5x5-athbt-zxzvl-ajrrc-6gmfn-6yk23-fzzlj-tt2yi-kps");

    // Call the backend function
    const response = await ref_backend.enrollPlayer(["text"]);

    // Handle the response
    const [success, message] = response;
    if (success) {
        greeting.value = message;
    } else {
        greeting.value = "Enrollment failed: " + message;
    }

    console.log("Response:", response);
  } catch (error) {
    console.error("Error enrolling player:", error);
    greeting.value = `An error occurred: ${error.message}`;
  }
}


async function handleSubmit(e) {
  e.preventDefault();
  const target = e.target;
  const name = target.querySelector('#name').value;
enrollPlayer("your-signup-code");

}
</script>

<template>
  <main>
    <img src="/logo2.svg" alt="DFINITY logo" />
    <br />
    <br />
    <form action="#" @submit="handleSubmit">
      <label for="name">Enter your name: &nbsp;</label>
      <input id="name" alt="Name" type="text" />
      <button type="submit">Click Me!</button>
    </form>
    <section id="greeting">{{ greeting }}</section>
  </main>
</template>
