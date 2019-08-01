// Here be JS

//const CHARS = "ABCČĆDĐEFGHIJKLMNOPQRSŠTUVWXYZŽabcčćdđefghijklmnopqrsštuvwxyzžАБВГҐДЂЕЁЄЖЗЅИІЇЙЈКЛЉМНЊОПРСТЋУЎФХЦЧЏШЩЪЫЬЭЮЯабвгґдђеёєжзѕиіїйјклљмнњопрстћуўфхцчџшщъыьэюяĂÂÊÔƠƯăâêôơư1234567890‘?’“!”(%)[#]{@}/&\<-+÷×=>®©$€£¥¢:;,.*^_~`|"
const CHARS = "ABCČĆDĐEFGHIJKLMNOPQRSŠTUVWXYZŽabcčćdđefghijklmnopqrsštuvwxyzžАБВГҐДЂЕЁЄЖЗЅИІЇЙЈКЛЉМНЊОПРСТЋУЎФХЦЧЏШЩЪЫЬЭЮЯабвгґдђеёєжзѕиіїйјклљмнњопрстћуўфхцчџшщъыьэюяĂÂÊÔƠƯăâêôơư1234567890‘?’“!”(%)[#]{@}/&\<-+÷×=>®©$€£¥¢:;,.*^_~`|"

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
    if (window.location.pathname == '/') {
        showSplash(getNewSplash());
    }
    if (window.matchMedia('(max-width: 720px)').matches) {
        document.querySelector('.navbar > a').setAttribute('href', 'javascript:');
    }
    document.getElementById('links').addEventListener("transitionend", awaitDropdown, true);
    whereAmI();
})

function whereAmI() {
    var path = window.location.href;
    var links = document.querySelectorAll('#links > a');
    links.forEach((ln) => {
        if (ln.href == path) {
            ln.querySelector('h1').classList.add('lit');
        }
    });
}

function getNewSplash(old) {
    var writer = document.getElementById('splashes');
    var splashList = [...writer.querySelectorAll('p')];
    var newSplashes = splashList.filter(x => x.textContent != old);
    return newSplashes[Math.floor(Math.random() * newSplashes.length)].textContent;
}

function unlockSplash() {
    var writer = document.getElementById('splashes');
    // Clear splashes
    writer.querySelectorAll('span').forEach((spn, idx) => {
        spn.classList.remove('locked');
    })
}

// Because I hate your CPU
function dedupOrder() {
    var writer = document.getElementById('splashes');
    var spanList = writer.querySelectorAll('span');
    var spans = [...spanList];
    spans.sort((a,b) => a.style.order - b.style.order);
    spanList.forEach((spn) => {
        spn.style.order = spans.indexOf(spn);
    });
}

// I'd like to grow and shrink while locking in correct char, but I'm taking
// the easy way out for now.
function showSplash(splash) {
    var rate = 75;
    var showTime = 5000;
    var shuffleCount = 1;
    var writer = document.getElementById('splashes');
    var chars = writer.querySelectorAll('span');
    var lockedChars = writer.querySelectorAll('span.locked');
    // First check if the length needs adjusting...
    if (chars.length < splash.length) {
        growSplashChar(splash);
        shuffleSplashChars(shuffleCount);
        needsDedup = true;
        setTimeout(showSplash, rate, splash);
    } else if (chars.length > splash.length) {
        shrinkSplashChar();
        shuffleSplashChars(shuffleCount);
        needsDedup = true;
        setTimeout(showSplash, rate, splash);
    } else if (lockedChars.length < splash.length) {
        if (needsDedup) {
            dedupOrder();
            needsDedup = false;
        } else {
            //shuffleSplashChars(shuffleCount);
            lockSplashChar(splash);
        }
        setTimeout(showSplash, rate, splash);
    } else {
        // Change to the next splash
        setTimeout((_) => {unlockSplash(); showSplash(getNewSplash(splash))}, showTime);
    }
    // Cool down all of the added / swapped elements
    window.requestAnimationFrame(_ => {
        window.requestAnimationFrame(_ => {
            chars.forEach((el) => {
                el.classList.remove('hot');
            });
        });
    });
}

function growSplashChar(splash) {
    var writer = document.getElementById('splashes');
    var spanList = writer.querySelectorAll('span');
    var idx = Math.floor(Math.random() * splash.length);
    var chridx = Math.floor(Math.random() * CHARS.length);
    var span = document.createElement('span');
    var text = document.createTextNode(CHARS.charAt(chridx));
    span.appendChild(text);
    span.classList.add('hot');
    span.style.order = idx;
    writer.appendChild(span);
}

function shrinkSplashChar() {
    var writer = document.getElementById('splashes');
    var spanList = writer.querySelectorAll('span');
    var idx = Math.floor(Math.random() * spanList.length);
    writer.removeChild(spanList[idx]);
}

function shuffleSplashChars(count) {
    var writer = document.getElementById('splashes');
    var spanList = writer.querySelectorAll('span:not(.locked)');
    while (count > 0) {
        var span = spanList[Math.floor(Math.random() * spanList.length)];
        span.textContent = CHARS.charAt(Math.floor(Math.random() * CHARS.length));
        span.classList.add('hot');
        count--;
    }
}

function lockSplashChar(splash) {
    var writer = document.getElementById('splashes');
    var spanList = writer.querySelectorAll('span:not(.locked)');
    var span = spanList[Math.floor(Math.random() * spanList.length)];
    span.textContent = splash[span.style.order];
    span.classList.add('locked');
    span.classList.add('hot');
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
