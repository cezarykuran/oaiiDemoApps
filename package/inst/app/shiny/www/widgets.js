oaiiShinyApp = {
  init : function() {
    Shiny.addCustomMessageHandler(
      "oaiiShinyApp.textConsole",
      oaiiShinyApp.textConsole.messageHandler
    )
    Shiny.addCustomMessageHandler(
      "oaiiShinyApp.rDownload",
      oaiiShinyApp.rDownload.messageHandler
    )
  },

  scrollDown : function(id) {
    obj = document.getElementById(id)
    obj.scrollTop = obj.scrollHeight
  },

  download : function(filename, content) {
    let a = document.createElement('a');
    a.download = filename
    a.href = content
    a.click()
  },

  rDownload : {
    messageHandler : function(message) {
      oaiiShinyApp.download(
        message.filename,
        "data:text/plain;charset=utf-8," + encodeURIComponent(atob(message.content)))
    }
  },

  textConsole : {
    attachEvent : function(id, inputId) {
      $('#' + inputId)
        .data("textConsoleId", id)
        .on("keydown", oaiiShinyApp.textConsole.onkeydown)
    },
    onkeydown : function(e) {
      e = e || window.event;
      if (e.keyCode == 13 && !e.shiftKey) {
        e.preventDefault()
        $obj = $(e.target)
        Shiny.setInputValue($obj.data("textConsoleId"), $obj.val(), {priority: 'event'})
      }
    },
    messageHandler : function(message) {
      obj = document.getElementById(message.inputId)
      switch (message.command) {
        case 'enable':
          obj.disabled = false;
          obj.focus();
          break;
        case 'disable':
          obj.disabled = true;
          break;
        case 'reset':
          obj.value = "";
          break;
        default:
          console.log("unknown message ", message)
      }
    }
  },

  images : {
    container : {
      fsInOut : function(obj) {
        $obj = $(obj)
        if($obj.hasClass("oaii-imagesSetImageContainerFS")) {
          $obj.removeClass("oaii-imagesSetImageContainerFS")
          $obj.parent().css("height", "")
        }
        else {
          $obj.parent().css("height", $obj.parent().height())
          $obj.addClass("oaii-imagesSetImageContainerFS")
        }
      },
      download : function(obj, e, filename = "image.png") {
        e = event || window.event
        e.stopPropagation()
        oaiiShinyApp.download(
          filename,
          $(obj).parent().children("img").attr("src")
        )
      }
    },

    edit : {
      $container : null,
      canvasFile : null,
      ctxFile : null,
      ctxDraw : null,
      imgFileId : null,
      imgFileW : null,
      imgFileH: null,
      colorDraw : null,

      init : function(idContainer, idImg) {
        oaiiShinyApp.images.edit.$container = $(document.getElementById(idContainer))
        oaiiShinyApp.images.edit.imgFileId = idImg

        $canvas = oaiiShinyApp.images.edit.$container.children("canvas")
        oaiiShinyApp.images.edit.canvasFile = $canvas.get(0)
        oaiiShinyApp.images.edit.ctxFile = $canvas.get(0).getContext('2d')
        oaiiShinyApp.images.edit.ctxDraw = $canvas.get(1).getContext('2d')

        Shiny.addCustomMessageHandler(
          "oaiiShinyApp.images.edit",
          oaiiShinyApp.images.edit.messageHandler
        )
        $(window).on("resize", oaiiShinyApp.images.edit.resize)
        oaiiShinyApp.images.edit.$container
          .on('mousedown', oaiiShinyApp.images.edit.start)
          .on('mouseup', oaiiShinyApp.images.edit.stop)
      },

      messageHandler : function(message) {
        switch (message.cmd) {
          case 'resize':
            oaiiShinyApp.images.edit.resize()
            break

          case 'file':
            var image = new Image();
            image.onload = function() {
              oaiiShinyApp.images.edit.imgFileW = image.width
              oaiiShinyApp.images.edit.imgFileH = image.height

              oaiiShinyApp.images.edit.ctxFile.reset()
              oaiiShinyApp.images.edit.$container.children("canvas")
                .prop("width", image.width)
                .prop("height", image.height)
              oaiiShinyApp.images.edit.ctxFile.fillStyle = "#fff"
              oaiiShinyApp.images.edit.ctxFile.drawImage(image, 0, 0)
              oaiiShinyApp.images.edit.ctxFile.globalCompositeOperation = "destination-out"
            }
            image.src = "data:image/png;base64," + message.data
            break

          case 'colorBg':
            oaiiShinyApp.images.edit.$container.css("background-color", message.data)
            break

          case 'colorDraw':
            oaiiShinyApp.images.edit.colorDraw = message.data
            break

          default:
            console.log("oaiiShinyApp.images.edit.messageHandler unknown message: " + message)
        }
      },

      resize : function() {
        $container = oaiiShinyApp.images.edit.$container
        $container
          .height($container.width())
          .children("canvas")
            .width($container.innerWidth())
            .height($container.innerHeight())
      },

      ctxXY : function(e) {
        var offset = oaiiShinyApp.images.edit.$container.offset()
        var scaleX = oaiiShinyApp.images.edit.imgFileW / oaiiShinyApp.images.edit.$container.innerWidth()
        var scaleY = oaiiShinyApp.images.edit.imgFileH / oaiiShinyApp.images.edit.$container.innerHeight()
        return {
          x: (e.clientX - offset.left + window.scrollX) * scaleX,
          y: (e.clientY - offset.top + window.scrollY) * scaleY
        }
      },

      move : function(e) {
        p = oaiiShinyApp.images.edit.ctxXY(e)
        oaiiShinyApp.images.edit.ctxDraw.fillRect(p.x, p.y, 2, 2)
        oaiiShinyApp.images.edit.ctxFile.lineTo(p.x, p.y)
      },

      start : function(e) {
        p = oaiiShinyApp.images.edit.ctxXY(e)
        oaiiShinyApp.images.edit.ctxDraw.fillStyle = oaiiShinyApp.images.edit.colorDraw;
        oaiiShinyApp.images.edit.ctxFile.beginPath()
        oaiiShinyApp.images.edit.ctxFile.moveTo(p.x, p.y)
        oaiiShinyApp.images.edit.$container.on('mousemove', oaiiShinyApp.images.edit.move);
      },

      stop : function(e) {
        oaiiShinyApp.images.edit.$container.off('mousemove', oaiiShinyApp.images.edit.move);
        oaiiShinyApp.images.edit.ctxFile.closePath()
        oaiiShinyApp.images.edit.ctxFile.fill()
        oaiiShinyApp.images.edit.ctxDraw.reset()

        Shiny.setInputValue(
          oaiiShinyApp.images.edit.imgFileId,
          oaiiShinyApp.images.edit.canvasFile.toDataURL("image/png")
        )
      }
    }
  },

  tableBtn : function(n, v) {
    Shiny.setInputValue(n, v)
  }
}
oaiiShinyApp.init()
