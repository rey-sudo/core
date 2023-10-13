<template>
    <div class="css-p6">
        <div class="css-p6-w">
            <div class="css-p6-w-grid">
                <div class="css-p6-w-grid-item">
                    <img src="@/pages/land/assets/avatar2.png" alt="">
                    <span>Grigore Roșu</span>
                </div>
                <div class="css-p6-w-grid-item">
                    <img src="@/pages/land/assets/avatar4.png" alt="">
                    <span>Xhinsu Dong</span>
                </div>
                <div class="css-p6-w-grid-item">
                    <img src="@/pages/land/assets/avatar.jpeg" alt="">
                    <span>Juan C. Rey</span>
                </div>
                <div class="css-p6-w-grid-item">
                    <img src="@/pages/land/assets/avatar3.png" alt="">
                    <span>Silvia Barredo</span>
                </div>
            </div>
        </div>
    </div>
</template>

<script>
export default {

}
</script>

<style lang="css" scoped>
.css-p6 {
    display: flex;
    justify-content: center;
    height: 100%;
}

.css-p6-w {
    width: 90%;
    height: 100%;
    border-bottom: none;
    display: flex;
    justify-content: center;
    align-items: center;
}

.css-p6 .css-p6-w .css-p6-w-grid {
    display: grid;
    grid-template-columns: repeat(4, 1fr);
    grid-gap: 1rem;
}

.css-p6 .css-p6-w .css-p6-w-grid .css-p6-w-grid-item {
    padding: 0.5rem;
    width: 300px;
    text-align: center;
    height: 300px;
    border: 1px solid var(--border-b);
    transition: 0.2s;
    background-clip: content-box;
    display: flex;
    flex-direction: column;
    text-align: start;
    justify-content: center;
    box-sizing: border-box;
    position: relative;
    border-radius: 50%;
    box-shadow: var(--button-shadow);
}

.css-p6 .css-p6-w .css-p6-w-grid .css-p6-w-grid-item span {
    width: inherit;
    position: absolute;
    z-index: 2;
    font-weight: 500;
    bottom: -3rem;
    text-align: center;
    font-size: var(--text-size-c);
}

.css-p6 .css-p6-w .css-p6-w-grid .css-p6-w-grid-item img {
    border-radius: 50%;
    -o-object-fit: cover;
    object-fit: cover;
    filter: grayscale(1);
    border: 1px solid rgb(0 0 0);
}

.css-p6 .css-p6-w .css-p6-w-grid .css-p6-w-grid-item div:nth-child(1) {
    display: flex;
    align-items: center;
    justify-content: flex-start;

}

.css-p6 .css-p6-w .css-p6-w-grid .css-p6-w-grid-item div:nth-child(2) {
    color: var(--text-a);
    font-weight: 500;
    margin-top: 1rem;
}

.css-p6 .css-p6-w .css-p6-w-grid .css-p6-w-grid-item div:nth-child(3) {
    color: var(--text-b);
    margin-top: 1rem;
}

.css-p6 .css-p6-w .css-p6-w-grid .css-p6-w-grid-item:hover {
    background: linear-gradient(to bottom, rgba(255, 255, 255, 0) 0%, rgba(255, 255, 255, .06) 100%);
}
</style>