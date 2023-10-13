<template>
    <div class="css-p7">
        <footer class="footer">
            <div class="footer-grid">
                <div class="footer-grid-item">
                    <div class="footer-grid-item-title">About us</div>
                    <a href="http://" target="_blank" rel="noopener noreferrer">Github</a>
                    <a href="http://" target="_blank" rel="noopener noreferrer">Telegram</a>
                    <a href="http://" target="_blank" rel="noopener noreferrer">Discord</a>
                    <a href="http://" target="_blank" rel="noopener noreferrer">Twitter</a>
                    <a href="http://" target="_blank" rel="noopener noreferrer">LinkedIn</a>
                </div>
                <div class="footer-grid-item">
                    <div class="footer-grid-item-title">Explore</div>
                    <a href="http://" target="_blank" rel="noopener noreferrer">Sandbox</a>
                    <a href="http://" target="_blank" rel="noopener noreferrer">Documentation</a>
                    <a href="http://" target="_blank" rel="noopener noreferrer">Schemes</a>
                    <a href="http://" target="_blank" rel="noopener noreferrer">ARKA token</a>
                    <a href="http://" target="_blank" rel="noopener noreferrer">ARKAGOV token</a>
                    <a href="http://" target="_blank" rel="noopener noreferrer">Research</a>
                </div>
                <div class="footer-grid-item">
                    <div class="footer-grid-item-title">Privacy policy</div>
                    <a href="http://" target="_blank" rel="noopener noreferrer">Data collection</a>
                    <a href="http://" target="_blank" rel="noopener noreferrer">Data treatment</a>
                    <a href="http://" target="_blank" rel="noopener noreferrer">Cookies policy</a>
                </div>
                <div class="footer-grid-item">
                    <div class="footer-grid-item-title">FAQ</div>
                    <a href="http://" target="_blank" rel="noopener noreferrer">Tokenomics</a>
                    <a href="http://" target="_blank" rel="noopener noreferrer">Decentralization level</a>
                    <a href="http://" target="_blank" rel="noopener noreferrer">Security</a>
                    <a href="http://" target="_blank" rel="noopener noreferrer">License</a>
                    <a href="http://" target="_blank" rel="noopener noreferrer">Roadmap</a>
                    <a href="http://" target="_blank" rel="noopener noreferrer">Fund</a>
                </div>
            </div>
            <div class="footer-bottom">Arka protocol LLC. 2023</div>
        </footer>
    </div>
</template>

<script>
export default {
}
</script>

<style lang="css" scoped>
.css-p7 {
    display: flex;
    justify-content: center;
    height: 100%;
    background-image: url('../assets/wave-2.svg');
    background-size: cover;
    background-repeat: no-repeat;
    background-position-y: 6rem;
}

.footer {
    display: flex;
    width: 90%;
    height: 100%;
    display: flex;
    justify-content: center;
    align-items: center;
    flex-direction: column;
}

.footer  .footer-grid {
    display: grid;
    grid-template-columns: repeat(4, 1fr);
    grid-gap: 1rem;
    padding: 1rem;
}

.footer  .footer-grid .footer-grid-item {
    padding: 1rem;
    width: 300px;
    background-clip: content-box;
    display: flex;
    flex-direction: column;
    text-align: start;
    box-sizing: border-box;
    position: relative;
    align-items: flex-start;
    justify-content: flex-start;
    line-height: 32px;
}

.footer .footer-grid .footer-grid-item a {
    color: var(--text-b);
}

.footer  .footer-grid .footer-grid-item .footer-grid-item-title {
    font-weight: 500;
    text-align: start;
    text-transform: capitalize;
    font-size: var(--text-size-b);
    color: var(--text-a);
}

.footer .footer-bottom {
    max-width: 1296px;
    display: flex;
    align-items: center;
    width: inherit;
    align-items: center;
    justify-content: space-between;
    padding: 26px 0;
    position: relative;
    border-top: 1px solid var(--border-b);
    font-size: var(--text-size-a);
    color: var(--text-b);
}

</style>