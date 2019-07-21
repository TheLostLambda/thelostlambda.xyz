// Here be JS

// Wait for the DOM to load before kicking things off
// Split this into several files...
document.addEventListener('DOMContentLoaded', function(event) {
    var logo = `
          .//.            .//.          
          .//.            .//.          
          .//-..        ..-//.          
          ./////        /////.          
                                        
--------       \`.------.\`       --------
-----://     .:///::::///:.     //:-----
     -//   \`://-\`      \`-//:\`   //-     
           ://\`          \`//:           
          \`//-            -//\`          
          \`//-            -//\`          
           ://\`          \`//:           
     -//   \`://-\`      \`-//:\`   //-     
-----://     .:///::::///:.     //:-----
--------       \`.------.\`       --------
                                        
          ./////        /////.          
          .//-..        ..-//.          
          .//.            .//.          
          .//.            .//.
`;
    console.log(logo);
    console.log('Oi! What you doin pokin round ere?');
    // I should add some checking for if these apply to the page being loaded
    showSplash();
    setInterval(showSplash, 5000);
    document.querySelector('.navbar > a').setAttribute('href', 'javascript:');
    document.getElementById('links').addEventListener("transitionend", awaitDropdown, true);
})

function showSplash() {
    var splashList = document.querySelectorAll('#splashes > p:not(.show)');
    var shown = document.querySelector('.show');
    if (shown != null) {
        shown.classList.remove('show');
    }
    var splash = splashList[Math.floor(Math.random() * splashList.length)];
    if (splash != null) {
        splash.classList.add('show');
    }
}

function awaitDropdown() {
    var linkHome = document.querySelector('.navbar > a');
    if (linkHome.getAttribute('href') === '/') {
        linkHome.setAttribute('href', 'javascript:');
    } else {
        linkHome.setAttribute('href', '/')
    }
}
