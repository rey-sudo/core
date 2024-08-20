<template>
  <div class="locking">
    <div class="subtitle">Provide shipping information to the seller.</div>

    <Button class="actived">
      <span>
        Paid
      </span>
    </Button>

    <Button class="cancel">
      <span>
        Return
      </span>
    </Button>
  </div>
</template>

<script>

export default {

};
</script>

<style lang="css" scoped>
.locking {
  display: flex;
  flex-direction: column;
}

.locking button {
  border: 1px solid var(--primary-c);
  background: var(--primary-c);
  font-size: var(--text-size-b);
  padding: 0.5rem 1rem;
  width: 100px;
  justify-content: center;
  font-weight: 600;
  color: var(--text-w);
  margin-right: 1rem;
  border-radius: 4px;
  cursor: pointer;
  margin-top: 1rem;
  display: flex;
  align-items: center
}

.locking button.actived {
  background: var(--green-a);
  border: 1px solid var(--green-a);
  pointer-events: none;

}

.locking button.cancel {
  background: var(--red-a);
  border: 1px solid var(--red-a);
}


.subtitle {
  font-size: var(--text-size-c);
  text-align: left;
  margin-top: 0.5rem;
}
</style>
