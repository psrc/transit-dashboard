$(document).ready(function() {
  
  //left panel categories changes color/design when clicked
  $(".focus").focusin(function() {
    $(this).css("border-bottom", "2px solid #a9a9a9")
    $(this).css("border-left", "4px solid #EC9B21")
    $(".source_url", this).css("color", "#2F3030")
    
  });
  $(".focus").focusout(function() {
    $(this).css("border-left", "0px solid #dedede")
    $(this).css("border-bottom", "2px solid #dedede")
    $(".source_url", this).css("color", "#00716C")

  });
  
  // left panel accordion header with links
  $(".panel-heading").focusin(function() {
  $(this).css("border-bottom", "2px solid #a9a9a9")
  $(this).css("border-left", "4px solid #EC9B21")
  $(".source_url", this).css("color", "#2F3030")
  
});
$(".panel-heading").focusout(function() {
  $(this).css("border-left", "0px solid #dedede")
  $(this).css("border-bottom", "2px solid #dedede")
  $(".source_url", this).css("color", "#00716C")
  
});

// left panel accordion panels
  $(".links-container").focusin(function() {
  $(this).css("border-bottom", "2px solid #a9a9a9")
  $(this).css("border-left", "4px solid #EC9B21")
  $(".links", this).css("color", "#2F3030")
  
});
$(".links-container").focusout(function() {
  $(this).css("border-left", "0px solid #F2F2F2")
  $(this).css("border-bottom", "2px solid #dedede")
  $(".links", this).css("color", "#00716C")
  
});
  
  
  
});