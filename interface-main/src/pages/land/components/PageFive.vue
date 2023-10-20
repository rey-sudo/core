<template>
    <div class="css-p5" ref="observed5">

        <div class="css-p5-w">
            <div class="overlay">
                <div class="css-p5-w-top">Auditor pool</div>

                <div class="css-p5-w-title">Auditors from anywhere</div>

                <div class="css-p5-w-subtitle">
                    <p>Any member of the Cardano ecosystem can become an auditor if they meet the quality requirements and the
                        community's voting threshold.</p>
                </div>
            </div>

            <div id="cobeWrap" />

        </div>
    </div>
</template>

<script>
import createGlobe from "cobe";
import locations from '@/pages/land/utils/locations';

export default {
    mounted() {
        let phi = 0;

        let canvas = document.createElement('canvas');
        canvas.id = 'cobeCanvas';
        canvas.width = 900;
        canvas.height = 900;
        canvas.style.width = '900px';
        canvas.style.height = '900px';

        const cobeWrap = document.getElementById("cobeWrap");

        cobeWrap.appendChild(canvas)

        createGlobe(canvas, {
            devicePixelRatio: 2,
            width: 900 * 2,
            height: 900 * 2,
            phi: 0,
            theta: 0,
            dark: 1,
            diffuse: 4,
            mapSamples: 16000,
            mapBrightness: 1,
            baseColor: [0.4, 0.4, 1],
            markerColor: [0.4, 0.4, 1],
            glowColor: [0.4, 0.4, 1],
            opacity: 0,
            markers: locations,
            onRender: (state) => {
                state.phi = phi;
                phi += 0.001;
            }
        })

    }

}

</script>

<style lang="css" scoped>
::v-deep(canvas) {
    top: 50%;
    left: 0;
    position: relative;
    right: 0;
    z-index: 1;
}

.css-p5 {
    display: flex;
    justify-content: center;
    height: 100%;
    overflow: hidden;
}


.css-p5 .css-p5-w {
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
    top: 20vh;
    z-index: 2;
}


.css-p5 .css-p5-w .css-p5-w-top {
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

.css-p5 .css-p5-w .css-p5-w-title {
    font-size: var(--text-size-e);
    font-weight: 500;
    line-height: 82px;
    background: linear-gradient(180deg, #fff 0%, rgba(255, 255, 255, .7) 100%);
    -webkit-background-clip: text;
    background-clip: text;
    color: transparent;
    -webkit-text-fill-color: transparent;
}

.css-p5 .css-p5-w .css-p5-w-subtitle {
    font-size: var(--text-size-b);
    font-weight: 400;
    color: var(--text-b);
    margin: 12px auto 0;
    max-width: 50%;
    line-height: 24px;
}
</style>