<template>
    <div class="css-p4">

        <div class="css-p4-w">
            <div class="overlay">
                <div class="css-p4-w-top">Certifications</div>

                <div class="css-p4-w-title">Three levels of audit depth</div>

                <div class="css-p4-w-subtitle">
                    <p>The community can easily identify in the dapp store which projects are the most secure according to 3 levels of audit.</p>

                </div>

                <div class="css-p4-w-grid">
                    <div class="css-p4-w-grid-item">
                        <div> <img src="@/pages/land/assets/cert-1.svg" alt=""></div>
                        <div>Fundamental</div>
                        <div>In-depth analysis of the fundamental aspects</div>
                        <span />
                    </div>
                    <div class="css-p4-w-grid-item">
                        <div> <img src="@/pages/land/assets/cert-2.svg" alt=""></div>
                        <div>Tested</div>
                        <div>Manual and automatic testing of smart contract properties</div>
                        <span />
                    </div>
                    <div class="css-p4-w-grid-item">
                        <div> <img src="@/pages/land/assets/cert-3.svg" alt=""></div>
                        <div>Formal verification</div>
                        <div>Mathematical approaches for precisely validating smart contracts</div>
                        <span />
                    </div>
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
.css-p4 {
    display: flex;
    justify-content: center;
    height: 100%;
}


.css-p4 .css-p4-w {
    width: 90%;
    height: 100%;
    display: flex;
    justify-content: center;
    align-items: center;
    flex-direction: column;
}

.overlay {
    position: absolute;
    display: flex;
    flex-direction: column;
    align-items: center;
    justify-content: center;
}


.css-p4 .css-p4-w .css-p4-w-top {
    font-size: var(--text-size-b);
    font-weight: 400;
    isolation: isolate;
    overflow: hidden;
    align-items: center;
    -webkit-backdrop-filter: blur(6px);
    backdrop-filter: blur(6px);
    border-radius: 32px;
    box-shadow: var(--button-shadow);
    display: flex;
    margin: 0 auto 12px;
    padding: 6px 14px 6px 15px;
    position: relative;
    width: -moz-max-content;
    width: max-content;
    border: 1px solid var(--border-a);
    background: linear-gradient(to right, #FC72FF, #8F68FF, #487BFF, #2CD9FF, #2CFFCC);
    -webkit-background-clip: text;
    background-clip: text;
    background-size: 100% 100%;
    -webkit-text-fill-color: transparent;
}

.css-p4 .css-p4-w .css-p4-w-title {
    font-size: var(--text-size-e);
    font-weight: 500;
    line-height: 82px;
    background: linear-gradient(180deg, #fff 0%, rgba(255, 255, 255, .7) 100%);
    -webkit-background-clip: text;
    background-clip: text;
    color: transparent;
    -webkit-text-fill-color: transparent;
}

.css-p4 .css-p4-w .css-p4-w-subtitle {
    font-size: var(--text-size-b);
    font-weight: 400;
    color: var(--text-b);
    margin: 12px auto 0;
    max-width: 50%;
    line-height: 24px;
}


.css-p4 .css-p4-w .css-p4-w-grid {
    display: grid;
    grid-template-columns: repeat(3, 1fr);
    grid-gap: 0px;
    margin-top: 1rem;
}

.css-p4 .css-p4-w .css-p4-w-grid .css-p4-w-grid-item {
    padding: 1.75rem;
    width: 300px;
    border: 1px solid var(--border-b);
    transition: 0.2s;
    background-clip: content-box;
    display: flex;
    flex-direction: column;
    text-align: start;
    box-sizing: border-box;
    position: relative;
    border-radius: 16px;
    margin: 0 1rem;
    align-items: center;
    justify-content: flex-start;
    line-height: 24px;
}

.css-p4 .css-p4-w .css-p4-w-grid .css-p4-w-grid-item span {
    width: 20px;
    height: inherit;
    position: absolute;
    z-index: 2;
    top: 0;
    left: calc(50% - 10px);
    border-top: 1px solid transparent;
    border-bottom: 1px solid var(--border-a);

}

.css-p4 .css-p4-w .css-p4-w-grid .css-p4-w-grid-item img {
    height: 50px
}

.css-p4 .css-p4-w .css-p4-w-grid .css-p4-w-grid-item div:nth-child(1) {
    display: flex;
    align-items: center;
    justify-content: flex-start;

}

.css-p4 .css-p4-w .css-p4-w-grid .css-p4-w-grid-item div:nth-child(2) {
    color: var(--text-a);
    font-weight: 500;
    margin-top: 1rem;
}

.css-p4 .css-p4-w .css-p4-w-grid .css-p4-w-grid-item div:nth-child(3) {
    color: var(--text-b);
    margin-top: 1rem;
    text-align: center;
}

.css-p4 .css-p4-w .css-p4-w-grid .css-p4-w-grid-item:hover {
    background: linear-gradient(to bottom, rgba(255, 255, 255, 0) 0%, rgba(255, 255, 255, .06) 100%);
}
</style>