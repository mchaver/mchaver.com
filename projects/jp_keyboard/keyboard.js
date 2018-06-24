/* load wasm file */
function fetchAndInstantiate(url, importObject = {}) {
  return fetch(url)
    .then(response => response.arrayBuffer())
    .then(bytes => WebAssembly.instantiate(bytes, importObject))
    .then(results => results.instance);
}

function copyCStr(module, ptr) {
  let orig_ptr = ptr;
  const collectCString = function* () {
    let memory = new Uint8Array(module.memory.buffer);
    while (memory[ptr] !== 0) {
      if (memory[ptr] === undefined) { throw new Error('Tried to read undef mem') }
      yield memory[ptr]
      ptr += 1
    }
  }

  const buffer_as_u8 = new Uint8Array(collectCString())
  const utf8Decoder = new TextDecoder('UTF-8');
  const buffer_as_utf8 = utf8Decoder.decode(buffer_as_u8);
  module.dealloc_str(orig_ptr);
  return buffer_as_utf8
}

function getStr(module, ptr, len) {
  const getData = function* (ptr, len) {
    let memory = new Uint8Array(module.memory.buffer);
    for (let index = 0; index < len; index++) {
      if (memory[ptr] === undefined) { throw new Error(`Tried to read undef mem at ${ptr}`) }
      yield memory[ptr + index]
    }
  }

  const buffer_as_u8 = new Uint8Array(getData(ptr / 8, len / 8));
  const utf8Decoder = new TextDecoder('UTF-8');
  const buffer_as_utf8 = utf8Decoder.decode(buffer_as_u8);
  return buffer_as_utf8;
}

function newString(module, str) {
  const utf8Encoder = new TextEncoder('UTF-8');
  let string_buffer = utf8Encoder.encode(str)
  let len = string_buffer.length
  let ptr = module.alloc(len + 1)

  let memory = new Uint8Array(module.memory.buffer);
  for (i = 0; i < len; i++) {
    memory[ptr + i] = string_buffer[i]
  }

  memory[ptr + len] = 0;

  return ptr
}

request = new XMLHttpRequest();
request.open('GET', 'romaji.gc.wasm');
request.responseType = 'arraybuffer';
request.send();

var exports;
var romaji_to_hiragana;
var romaji_to_hiragana_safe;

request.onload = function() {
  console.log('loaded');
  var bytes = request.response;
  WebAssembly.instantiate(bytes).then(results => {
    const instance = results.instance;
    exports = instance.exports;

    romaji_to_hiragana = s => {
      let outptr = exports.romaji_to_hiragana(newString(exports, s));
      return copyCStr(exports, outptr);
    };

    romaji_to_hiragana_safe = s => {
      let outptr = exports.romaji_to_hiragana_safe(newString(exports, s));
      return copyCStr(exports, outptr);
    };
  });
};

function convert(event) {
  document.getElementById('result').innerHTML = romaji_to_hiragana(event.value);
}

function preserve(event) {
  document.getElementById('preserve-output').innerHTML = romaji_to_hiragana_safe(event.value);
}

function overwrite(event) {
  const start = event.selectionStart;
  const end = event.selectionEnd;

  const romaji = event.value;
  const hiragana = romaji_to_hiragana_safe(event.value);
  event.value = romaji_to_hiragana_safe(event.value);

  var d = hiragana.length - romaji.length

  // restore from variables...
  event.setSelectionRange(start + d, end + d);
}

function getParameterByName(name, url) {
  if (!url) url = window.location.href;
  name = name.replace(/[\[\]]/g, "\\$&");
  var regex = new RegExp("[?&]" + name + "(=([^&#]*)|&|#|$)"),
      results = regex.exec(url);
  if (!results) return null;
  if (!results[2]) return '';
  return decodeURIComponent(results[2].replace(/\+/g, " "));
}

window.onload = function() {
  document.getElementById('overwrite').focus();
  document.getElementById('overwrite-text-button').checked = true;
  document.getElementById('en-button').checked = true;
  document.getElementById('small-input-button').checked = true;
  document.getElementById('hide-uncommon').checked = true;

  var lang = getParameterByName('lang');

  if (lang === 'jp') {
    document.getElementById('en-button').checked = false;
    document.getElementById('jp-button').checked = true;
    showJapanese();
  } else {
    document.getElementById('en-button').checked = true;
    document.getElementById('jp-button').checked = false;
    showEnglish();
  }

  var divsToHide = document.getElementsByClassName('uncommon');
  for (var i = 0; i < divsToHide.length; i++) {
    divsToHide[i].style.display = 'none';
  }

}

function showJapanese() {
  var divsToHide = document.getElementsByClassName('en');
  for (var i = 0; i < divsToHide.length; i++) {
    divsToHide[i].removeAttribute('lang');
  }
  
  var divsToShow = document.getElementsByClassName('jp');
  for (var i = 0; i < divsToShow.length; i++) {
    divsToShow[i].lang = 'jp';
  }
}

function showEnglish() {
  var divsToHide = document.getElementsByClassName('jp');
  for (var i = 0; i < divsToHide.length; i++) {
    divsToHide[i].removeAttribute('lang');
  }
  
  var divsToShow = document.getElementsByClassName('en');
  for (var i = 0; i < divsToShow.length; i++) {
    divsToShow[i].lang = 'en';
  }
}

var inputStyle = 'overwrite';
var inputSize = 'small';

function setOverwrite(r) {
  
  if (inputStyle === 'overwrite') {
    var overwrite_group = document.getElementById('overwrite-group');
    var preserve_group = document.getElementById('preserve-group');
    overwrite_group.style.display = '';
    preserve_group.style.display = 'none';

  } else if (inputStyle === 'preserve') {
    var overwrite_group = document.getElementById('overwrite-group');
    var preserve_group = document.getElementById('preserve-group');
    overwrite_group.style.display = 'none';
    preserve_group.style.display = '';
  }
}

function setLanguage(r) {
  if (r.value === 'en') {
    showEnglish();
  } else if (r.value === 'jp') {
    showJapanese();
  }
}

function setInput(r, typ) {
  if (typ === 'style') {
    inputStyle = r.value;

    if (inputStyle === 'overwrite') {
      var overwrite_group = document.getElementById('overwrite-group');
      var preserve_group = document.getElementById('preserve-group');
      overwrite_group.style.display = '';
      preserve_group.style.display = 'none';

    } else if (inputStyle === 'preserve') {
      var overwrite_group = document.getElementById('overwrite-group');
      var preserve_group = document.getElementById('preserve-group');
      overwrite_group.style.display = 'none';
      preserve_group.style.display = '';
    }
  } else if (typ === 'size') {
    inputSize = r.value;
  }
  
  var overwrite = document.getElementById('overwrite');
  var preserve_input = document.getElementById('preserve-input');

  if (inputSize === 'small') {
    overwrite.rows = '1';
    preserve_input.rows = '1';
    overwrite.style.height = '1.4em';
    preserve_input.style.height = '1.4em';
  } else {
    overwrite.rows = '4';
    preserve_input.rows = '4';
    overwrite.style.height = '5.6em';
    preserve_input.style.height = '5.6em';
  }

  var nav = document.getElementById('nav');
  var kanatable = document.getElementById('kanatable');
  var em = parseFloat(getComputedStyle(nav).fontSize);
  kanatable.style['margin-top'] = String(nav.clientHeight + (em * 4.9)) + 'px';
}

function showUncommon(r) {
  var divsToHide = document.getElementsByClassName('uncommon');

  if (r.value === 'hide') {
    for (var i = 0; i < divsToHide.length; i++) {
      divsToHide[i].style.display = 'none';
    }
  } else if (r.value === 'show') {
    for (var i = 0; i < divsToHide.length; i++) {
      divsToHide[i].style.display = '';
    }
  }
}
