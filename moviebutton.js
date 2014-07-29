var playBtn = document.getElementById('play-btn');
var pauseBtn = document.getElementById('pause-btn');
var muteBtn = document.getElementById('mute-btn');
var unmuteBtn = document.getElementById('unmute-btn');
var stopBtn = document.getElementById('stop-btn');
var replayBtn = document.getElementById('replay-btn');

playBtn.addEventListener('click', pausePlayHandler, false);
pauseBtn.addEventListener('click', pausePlayHandler, false);
muteBtn.addEventListener('click', muteUnmuteHandler, false);
unmuteBtn.addEventListener('click', muteUnmuteHandler, false);
stopBtn.addEventListener('click', stopHandler, false);
replayBtn.addEventListener('click', replayHandler, false);

function pausePlayHandler(e) {
   if (video1.paused) {
       // If paused, then play
       video1.play();
        // Show pause button and hide play button
       pauseBtn.style.visibility = 'visible';
       playBtn.style.visibility = 'hidden';
   } else {
       // If playing, then pause
       video1.pause();
       // Show play button and hide pause button
       pauseBtn.style.visibility = 'hidden';
       playBtn.style.visibility = 'visible';
   }
}

function muteUnmuteHandler(e) {
   if (video1.volume == 0.0) {
       // If muted, then turn it on
       video1.volume = 1.0;
       // Show mute button and hide unmute button
       muteBtn.style.visibility = 'visible';
       unmuteBtn.style.visibility = 'hidden';
   } else {
       // If unmuted, then turn it off
       video1.volume = 0.0;
       // Show unmute button and hide mute button
       muteBtn.style.visibility = 'hidden';
       unmuteBtn.style.visibility = 'visible';
   }
}

function stopHandler(e) {
   // There is no stop method for HTML5 video
   // As a workaround, pause the video
   // and set currentTime to 0
   video1.currentTime = 0;
   video1.pause();
   // Show or hide other video buttons accordingly
}

function replayHandler(e) {
   // There is no replay method for HTML5 video
   // As a workaround, set currentTime to 0
   // and play the video
   video1.currentTime = 0;
   video1.play();
   // Show or hide other video buttons accordingly
}
