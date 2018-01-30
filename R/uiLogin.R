
uiLoginModal <- function() {
  modalDialog(
    #div(style = 'text-align:center;',
    h3('Login'),
    
    textInput("login_email", "Email:"),    
    passwordInput("login_password", "Password:"),
    div(style = 'color: red; margin-bottom: 5px', textOutput('wrongPasswordMsg1')),  
    actionButton("doLogin", "Login", icon = icon("sign-in")),
   
    footer = tagList(
      actionButton("cancelLogin", "Cancel")
    )
  )
}

uiSignupModal <- function() {
  modalDialog(

    h3('Sign-up'),
  
    div(style = "margin-top: 25px; margin-bottom: 25px", 'If not already registered, sign-up by specifying an e-mail and password.'),
    textInput("signup_email", "Email:"),    
    passwordInput("signup_password", "Password:"),
    passwordInput("signup_password2", "Repeat password:"),
    div(style = 'color: red; margin-bottom: 5px', textOutput('wrongPasswordMsg2')),  
    actionButton("doSignup","Sign up", icon = icon("user-plus")),
    
    
    footer = tagList(
      actionButton("cancelSignup", "Cancel")
    )
  )
  
}