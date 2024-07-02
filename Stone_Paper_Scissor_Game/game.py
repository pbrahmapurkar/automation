import random

def get_computer_choice():
    return random.randint(1, 3)

def get_user_choice():
    option_dict = {1: 'Stone', 2: "Paper", 3: "Scissor"}
    user = int(input("Choose a Number: \n 1. Stone \n 2. Paper \n 3. Scissor \n Enter for Stone Paper Scissor: "))
    while user not in option_dict:
        print("Please enter a correct choice!")
        user = int(input("Choose a Number: \n 1. Stone \n 2. Paper \n 3. Scissor \n Enter for Stone Paper Scissor: "))
    return user

def determine_winner(user, computer):
    option_dict = {1: 'Stone', 2: "Paper", 3: "Scissor"}
    if user == computer:
        print(f"You both chose: {option_dict.get(computer)}. It's a draw.")
    elif (computer == 1 and user == 2) or (computer == 2 and user == 3) or (computer == 3 and user == 1):
        print(f"Computer chose {option_dict.get(computer)}. And you chose {option_dict.get(user)}. YOU WIN!")
    else:
        print(f"Computer chose {option_dict.get(computer)}. And you chose {option_dict.get(user)}. Computer WINS!")

def play_game():
    computer = get_computer_choice()
    user = get_user_choice()
    determine_winner(user, computer)
    play_again = input("Do you want to play again? (Y/N): ").strip().lower()
    if play_again == 'y':
        play_game()
    else:
        print("Thank you for playing!")
        # Print a pattern when the user decides to stop playing
        for i in range(1, 7):
            print(" " * (6 - i), end="")
            print("*" * (2 * i - 1), end="")
            print("\n")

if __name__ == "__main__":
    play_game()
