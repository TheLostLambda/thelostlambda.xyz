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
    if (window.matchMedia('(max-width: 720px)').matches) {
        document.querySelector('.navbar > a').setAttribute('href', 'javascript:');
    }
    document.getElementById('links').addEventListener("transitionend", awaitDropdown, true);
})

function showSplash() {
    var splashList = document.querySelectorAll('#splashes > p:not(.show)');
    var splash = splashList[Math.floor(Math.random() * splashList.length)].textContent;
    var writer = document.querySelector('#splashes');
    // Clear splashes
    writer.querySelectorAll('span').forEach((spn) => {
        writer.removeChild(spn);
    })
    // Add splash
    for (var i = 0; i < splash.length; i++) {
        var span = document.createElement('span');
        var text = document.createTextNode(splash.charAt(i));
        span.appendChild(text);
        writer.appendChild(span);
    }
}

function awaitDropdown() {
    if (window.matchMedia('(max-width: 720px)').matches) {
        var linkHome = document.querySelector('.navbar > a');
        if (linkHome.getAttribute('href') === '/') {
            linkHome.setAttribute('href', 'javascript:');
        } else {
            linkHome.setAttribute('href', '/')
        }
    }
}
