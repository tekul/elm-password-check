const app = Elm.Main.init({
    node: document.getElementById("app"),
    flags: {}
});

app.ports.checkPassword.subscribe(function(password) {
    var report = zxcvbn(password);
    app.ports.passwordChecked.send(report);
});
