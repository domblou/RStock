$Host.UI.RawUI.WindowTitle = "RStock"

Set-Location "C:\Dev\RStock"

Clear-Host
Write-Host "========================================" -ForegroundColor Cyan
Write-Host "             RStock" -ForegroundColor Cyan
Write-Host "========================================" -ForegroundColor Cyan
Write-Host ""
Write-Host "Démarrage de RStock..." -ForegroundColor Green
Write-Host "Répertoire : $PWD"
Write-Host ""

try {
    python -m streamlit run rstock/application/streamlit_app.py
}
catch {
    Write-Host ""
    Write-Host "ERREUR lors du démarrage de RStock :" -ForegroundColor Red
    Write-Host $_ -ForegroundColor Red
}

Write-Host ""
Write-Host "RStock est arrêté." -ForegroundColor Yellow
Read-Host "Appuyez sur Entrée pour fermer cette fenêtre"