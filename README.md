# formation-cobol-cics

## Installez Visual Studio Code (VSCode)

[Visual studio code](https://code.visualstudio.com/)

## Installez l'extension IBM Z Open Editor

[IBM Z Open Editor](https://marketplace.visualstudio.com/items?itemName=IBM.zopeneditor)

## Installez l'extension IBM-JCL

[IBM-JCL](https://marketplace.visualstudio.com/items?itemName=kelosky.ibm-jcl)

IBM Z Open Editor gère le jcl, mais si vous voulez une extension plus poussée

Nommez vos fichiers COBOL en **.cob** ou **.cbl**

Nommez vos fichiers JCL en **.jcl**

Nommez vos fichiers MAP en **.asm** (support basique)

Vous avez maintenant un support de cobol basique.

## Gestion des copies

Créer un dossier .vscode à la racine de votre projet

Créer un fichier settings.json dans ce dossier.

Copier coller le code suivant

```json
{
    "zopeneditor.propertygroups": [
        {
            "name": "local-cobol-files",
            "type": "local",
            "language": "cobol",
            "syslib": [
                "C:/my/path/to/my/copybook1",
                "C:/different/path/to/different/copybook"
            ],
        }
    ],
}
```

la **syslib** est un tableau de chemin vers vos copybook, vous pouvez en ajouter autant que vous voulez séparer par des virgules

Nommez vos fichiers COPY en **.cpy**.
