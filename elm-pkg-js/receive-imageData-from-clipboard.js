exports.init = async function init(app) {
  // https://ourcodeworld.com/articles/read/491/how-to-retrieve-images-from-the-clipboard-with-javascript-in-the-browser

  /**
   * This handler retrieves the images from the clipboard as a blob and returns it in a callback.
   *
   * @param pasteEvent
   * @param callback
   */
  function retrieveImageFromClipboardAsBlob(pasteEvent, callback) {
    if (pasteEvent.clipboardData == false) {
      if (typeof callback == "function") {
        callback(undefined);
      }
    }

    var items = pasteEvent.clipboardData.items;

    if (items == undefined) {
      if (typeof callback == "function") {
        callback(undefined);
      }
    }

    for (var i = 0; i < items.length; i++) {
      // Skip content if not image
      if (items[i].type.indexOf("image") == -1) continue;
      // Retrieve image on clipboard as blob
      var blob = items[i].getAsFile();

      if (typeof callback == "function") {
        callback(blob);
      }
    }
  }

  window.addEventListener(
    "paste",
    function (e) {
      // Handle the event
      retrieveImageFromClipboardAsBlob(e, function (imageBlob) {
        // If there's an image, display it in the canvas
        if (imageBlob) {
          var canvas = document.getElementById("pasteImageCanvas");
          var ctx = canvas.getContext("2d");

          // Create an image to render the blob on the canvas
          var img = new Image();

          // Once the image loads, render the img on the canvas
          img.onload = function () {
            // Update dimensions of the canvas with the dimensions of the image
            canvas.width = this.width;
            canvas.height = this.height;

            // Draw the image
            ctx.drawImage(img, 0, 0);
          };

          // Crossbrowser support for URL
          var URLObj = window.URL || window.webkitURL;

          var mimeType = imageBlob.type;
          var imageSize = imageBlob.size;

          // Creates a DOMString containing a URL representing the object given in the parameter
          // namely the original Blob
          img.src = URLObj.createObjectURL(imageBlob);

          const reader = new FileReader();
          reader.readAsDataURL(imageBlob);
          reader.onload = function () {
            //              const base64String = reader.result;
            //              data = { mimeType: mimeType,
            //                       imageSize: imageSize,
            //                       content: base64String}
            app.ports.copyUrlToClipboard.send(imageBlob);
          };
        }
      });
    },
    false
  );
};
