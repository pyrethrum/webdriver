module TestPages
  ( helloHtml
  ) where

import qualified Data.Text.Lazy as TL

helloHtml :: TL.Text
helloHtml =
  "<!DOCTYPE html>" <>
  "<html>" <>
  "<head>" <>
    "<meta charset='UTF-8'>" <>
    "<meta name='viewport' content='width=device-width, initial-scale=1.0'>" <>
    "<title>Test Server</title>" <>
    "<style>" <>
      "body { " <>
        "font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, sans-serif; " <>
        "background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); " <>
        "min-height: 100vh; " <>
        "display: flex; " <>
        "align-items: center; " <>
        "justify-content: center; " <>
        "margin: 0; " <>
        "padding: 20px; " <>
      "}" <>
      ".panel { " <>
        "background: white; " <>
        "border-radius: 12px; " <>
        "box-shadow: 0 20px 60px rgba(0,0,0,0.3); " <>
        "padding: 60px 80px; " <>
        "text-align: center; " <>
        "max-width: 600px; " <>
      "}" <>
      "h1 { " <>
        "color: #667eea; " <>
        "font-size: 3em; " <>
        "margin: 0; " <>
        "font-weight: 700; " <>
      "}" <>
      ".emoji { " <>
        "font-size: 4em; " <>
        "margin-bottom: 20px; " <>
      "}" <>
    "</style>" <>
  "</head>" <>
  "<body>" <>
    "<div class='panel'>" <>
      "<div class='emoji'>👋</div>" <>
      "<h1>Hello from Test Server!</h1>" <>
    "</div>" <>
  "</body>" <>
  "</html>"
