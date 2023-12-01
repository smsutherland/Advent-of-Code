#include <iostream>
#include <ctime>
#define NUM_CUPS 1000000

struct Node{
    unsigned int data;
    Node* next = nullptr;

    Node(unsigned int data_ = 0) : data(data_){}
};

int main(){
    Node* cups = new Node[NUM_CUPS+1];
    unsigned int first9[9] = {3,2,6,5,1,9,4,7,8};
    for(int i = 1; i < NUM_CUPS+1; i++){
        cups[i] = Node(i);
    }
    for(int i = 0; i < 8; i++){
        cups[first9[i]].next = &cups[first9[i+1]];
    }
    cups[8].next = &cups[10];
    for(unsigned int i = 10; i < NUM_CUPS; i++){
        cups[i].next = &cups[i+1];
    }
    cups[NUM_CUPS].next = &cups[3];

    Node* currentNode = &cups[3];
    unsigned int nodeSize = sizeof(Node);
    for(unsigned int i = 0; i < 10000000; i++){
        
        unsigned int currentCupNum = currentNode->data;

        Node* next1 = currentNode->next;
        Node* next2 = next1->next;
        Node* next3 = next2->next;

        currentNode->next = next3->next;

        Node* destination = nullptr;
        while(true){
            currentCupNum--;
            if(currentCupNum == 0)
                currentCupNum = NUM_CUPS;
            if(currentCupNum == next1->data || currentCupNum == next2->data || currentCupNum == next3->data)
                continue;
            destination = &cups[currentCupNum];
            break;
        }

        next3->next = destination->next;
        destination->next = next1;
        currentNode = currentNode->next;
    }

    Node* one = &cups[1];
    one = one->next;
    unsigned long long num = one->data;
    std::cout << num << std::endl;
    one = one->next;
    num *= one->data;
    std::cout << one->data << std::endl;
    std::cout << num << std::endl;

    delete[] cups;
}