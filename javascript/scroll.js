"use strict";

var windowHeight;
var scrollTS = 0;
var lastScroll = 0;

window.requestAnimationFrame (animateScroll);

function computeWindowHeight() {
  var myHeight = 0;
  if( typeof( window.innerWidth ) == 'number' ) {
    //Non-IE
    myHeight = window.innerHeight;
  } else if( document.documentElement && ( document.documentElement.clientHeight ) ) {
    //IE 6+ in 'standards compliant mode'
    myHeight = document.documentElement.clientHeight;
  } else if( document.body && ( document.body.clientHeight ) ) {
    //IE 4 compatible
    myHeight = document.body.clientHeight;
  }
  return (myHeight);
}

windowHeight = computeWindowHeight();

function getScroll() {
  var scrOfY = 0;
  if( typeof( window.pageYOffset ) == 'number' ) {
    //Netscape compliant
    scrOfY = window.pageYOffset;
  } else if( document.body && ( document.body.scrollLeft || document.body.scrollTop ) ) {
    //DOM compliant
    scrOfY = document.body.scrollTop;
  } else if( document.documentElement && ( document.documentElement.scrollLeft || document.documentElement.scrollTop ) ) {
    //IE6 standards compliant mode
    scrOfY = document.documentElement.scrollTop;
  }
  return (scrOfY);
}

function computeScroll () {
    var percentage = Math.round (getScroll() / (document.body.offsetHeight - windowHeight) * 100);
    return (percentage);
}

function animateScroll (timestamp) {
    if (timestamp - scrollTS >= 100) {
        scrollTS = timestamp;

        var newScroll = computeScroll();

        if (newScroll !== lastScroll) {
            lastScroll = newScroll;
            document.getElementById ("scrollBar").style.width = newScroll + "%";
        }

    }

    window.requestAnimationFrame (animateScroll);
}

document.getElementById("toc").addEventListener("mouseover", makeTOCTransparent);
document.getElementById("toc").addEventListener("mouseout", makeTOCOpaque);

function makeTOCTransparent (evt) {
  // document.getElementById("toc").getElementsByTagName("ul")[0].style.color = "red";
  document.getElementById("toc").getElementsByTagName("ul")[0].className = "tocHover";
};

function makeTOCOpaque (evt) {
  // document.getElementById("toc").getElementsByTagName("ul")[0].style.color = "rgba(255,255,255,1.0)";
  document.getElementById("toc").getElementsByTagName("ul")[0].className = "";
}
