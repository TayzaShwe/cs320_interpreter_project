# CS320 INTERPRETER PROJECT #
edit
This is the official repository for my Tic-Tac-Toe Game. 

### Table of Contents ###
1) Why I developed this tic-tac-toe game
2) How the game works
3) How the minimax algorithm works
4) Using PyQt5 and PyInstaller
5) Technologies involved 

## WHY I DEVELOPED THIS TRADING BOT ##

It's a pretty simple but fun game and I was initially just interested in how to create an algorithm that would be hard to beat. So, I coded it and even added a user interface!

## HOW MY TRADING BOT WORKS ##

You can play against three kinds of opponents:
* Player 2. Just takes in another input from user.
* Easy Bot. Randomizes the next move.
* Hard Bot. Uses the minimax algorithm to find optimal move.

[Demonstration video.](https://youtu.be/EiNxiaXhf8w)

## HOW THE MINIMAX ALGORITHM WORKS ##

The minimax algorithm is a recursive algorithm that simulates every possible move. The score that is returned when I win would be 1 and the score that is returned when my opponent wins is -1. The score is 0 when nobody has won yet and it is not a draw. We will refer to the number of moves as depth. The algorithm tries every available move at one depth and if it is my turn, picks the move with the highest score. The algorithm picks the lowest score for if it is my opponent's turn. The idea is that the algorithm picks the best move for me and the best move for the opponent since the lowest score is the best score for my opponent. It does this until all possible moves are calculated. If there are multiple highest scores, it arbitrarily picks a highest score. 

## USING PYQT5 AND PYINSTALLER ##

The first version I created used the terminal for user interface. I then switched to PyQt5. 
1) Download PyQt5
```
pip install pyqt5
pip install pyqt5-tools
```
2) Design the windows in designer. The app can be found inside the site-packages folder inside your python folder. This is PyQt5's drag-and-drop style application that allows you to build the interface. 
3) Save the design as a .ui file.
4) Convert the .ui file into a python file with the following command:
```
pyuic5 -x filename.ui -o filename.py
```
5) Edit the python file to add functionality. 
6) Download PyInstaller
```
pip install pyinstaller
```
7) Convert the python file into an executable file. Not that this converts to an executable file only compatible with your operating system. I used linux, so the executable file in this repository is only compatible with linux. Also note that the file size may be large since it adds all of PyQt5 into the executable file. There is no way around this issue at the moment.
```
pyinstaller --onefile filename.py
```
Now will you have the executable file! 

## TECHNOLOGIES INVOLVED ##

* Python 
* [PyQt5](https://pypi.org/project/PyQt5/)
* [PyInstaller](https://pyinstaller.org/en/stable/) 

