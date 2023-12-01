#include <iostream>
#include <vector>
#include <queue>

std::queue<int> results = std::queue<int>();

bool recursiveCombat(std::queue<int> player1, std::queue<int> player2);
void printQueue(std::queue<int> q);

int main(){
    std::queue<int> player1 = std::queue<int>();
    std::queue<int> player2 = std::queue<int>();
    player1.push(18);
    player1.push(19);
    player1.push(16);
    player1.push(11);
    player1.push(47);
    player1.push(38);
    player1.push(6);
    player1.push(27);
    player1.push(9);
    player1.push(22);
    player1.push(15);
    player1.push(42);
    player1.push(3);
    player1.push(4);
    player1.push(21);
    player1.push(41);
    player1.push(14);
    player1.push(8);
    player1.push(23);
    player1.push(30);
    player1.push(40);
    player1.push(13);
    player1.push(35);
    player1.push(46);
    player1.push(50);

    player2.push(39);
    player2.push(1);
    player2.push(29);
    player2.push(20);
    player2.push(45);
    player2.push(43);
    player2.push(12);
    player2.push(2);
    player2.push(37);
    player2.push(33);
    player2.push(49);
    player2.push(32);
    player2.push(10);
    player2.push(26);
    player2.push(36);
    player2.push(17);
    player2.push(34);
    player2.push(44);
    player2.push(25);
    player2.push(28);
    player2.push(24);
    player2.push(5);
    player2.push(48);
    player2.push(31);
    player2.push(7);

    // player1.push(9);
    // player1.push(2);
    // player1.push(6);
    // player1.push(3);
    // player1.push(1);

    // player2.push(5);
    // player2.push(8);
    // player2.push(4);
    // player2.push(7);
    // player2.push(10);

    recursiveCombat(player1, player2);

    printQueue(results);
    int score = 0;
    int maxSize = results.size();
    for(int i = 0; i < maxSize; i++){
        score += (maxSize-i)*results.front();
        results.pop();
    }
    std::cout << score << std::endl;
}

bool recursiveCombat(std::queue<int> player1, std::queue<int> player2){
    std::vector<std::queue<int>> prevRounds1 = std::vector<std::queue<int>>();
    std::vector<std::queue<int>> prevRounds2 = std::vector<std::queue<int>>();
    while(!(player1.empty() || player2.empty())){
        // std::cout << "player1: ";
        // printQueue(player1);
        // std::cout << "player2: ";
        // printQueue(player2);



        for(int i = 0; i < prevRounds1.size(); i++){
            if((player1 == prevRounds1[i]) && (player2 == prevRounds2[i])){
                results = player1;
                return true;
            }
        }

        prevRounds1.push_back(player1);
        prevRounds2.push_back(player2);

        int player1Card = player1.front();
        int player2Card = player2.front();
        player1.pop();
        player2.pop();

        bool winner = player1Card > player2Card;
        // std::cout << player1Card << " " << player1.size() << " " << player2Card << " " << player2.size() << std::endl;
        if((player1.size() >= player1Card) && (player2.size() >= player2Card)){
            // std::cout << "entering recursive combat" << std::endl;
            std::queue<int> newPlayer1 = std::queue<int>(player1);
            std::queue<int> newPlayer2 = std::queue<int>(player2);
            int playerSize = newPlayer1.size();
            for(int i = 0; i < playerSize; i++){
                int temp = newPlayer1.front();
                newPlayer1.pop();
                if(i < player1Card){
                    newPlayer1.push(temp);
                }
            }
            playerSize = player2.size();
            for(int i = 0; i < playerSize; i++){
                int temp = newPlayer2.front();
                newPlayer2.pop();
                if(i < player2Card){
                    newPlayer2.push(temp);
                }
            }

            winner = recursiveCombat(newPlayer1, newPlayer2);
        }
        if(winner){
            // std::cout << "player 1 won" << std::endl;
            player1.push(player1Card);
            player1.push(player2Card);
        }
        else{
            // std::cout << "player 2 won" << std::endl;
            player2.push(player2Card);
            player2.push(player1Card);
        }
    }
    if(player1.size() > 0){
        results = player1;
    }
    else{
        results = player2;
    }
    return player1.size() > 0;
}

void printQueue(std::queue<int> q){
    while(q.size() > 0){
        std::cout << q.front() << ", ";
        q.pop();
    }
    std::cout << std::endl;
}