from RBTree_PrettyPrint import *
import random
    
class RedBlackNode:
# attributes:
#   key: A value of None indicates an empty node. (Note: We'll skip the value attribute this time around.)
#   color: This should be a single-character string: "R" or "B"
#   left: a RedBlackNode
#   right: a RedBlackNode
#   parent: a RedBlackNode
    
    def __init__(self, key, color=None):
        self.key = key
        self.color = color
        # Sanity check: empty nodes must be black.
        if self.key is None and self.color != "B":
            raise ValueError("Empty children must be colored black.")
    
        self.left = None
        self.right = None
        self.parent = None
    
class RedBlackTree:
# attributes:
#   root: a RedBlackNode
#   methods: various.
    
    def __init__(self):
        self.root = RedBlackNode(None, "B")
    
        
    # ****************************************
    # Finding and Insertion
    # ****************************************
    
    
    # Return the node with the key.
    # Returns None if that key isn't found.    
    def find(self, key, node="root"):
        # If this function is called externally, it won't be called with a node.
        # So set the starting node to root.
        if (node == "root"): node = self.root
        # You can't find anything in an empty subtree.
        if (node.key is None): return None
        if (key == node.key): return node
        if (key < node.key): return self.find(key, node.left)
        if (key > node.key): return self.find(key, node.right)
 
    # Adds a node to the tree with given key.
    # A no-op if the key already exists.
    # Does not return anything.
    def insert(self, key, node=None):        
        # Handle an empty tree separately.
        if self.root.key is None:
            self.root.key = key
            self.root.color = "B"
            self.__setup_empty_children(self.root)
            return
        if (not node): node = self.root
        # At this point node is always a regular (non-empty) node.
        if (key == node.key):
            return
        if (key < node.key):
            if (node.left.key is None):
                node.left = RedBlackNode(key, "R")
                node.left.parent = node
                self.__setup_empty_children(node.left)
                self.__adjust_insert(node.left) 
            else:
                self.insert(key, node.left)
        if (key > node.key):
            if (node.right.key is None):
                node.right = RedBlackNode(key, "R")
                node.right.parent = node
                self.__setup_empty_children(node.right)
                self.__adjust_insert(node.right)
            else:
                self.insert(key,node.right)
     
    def __setup_empty_children(self, node):
        node.left = RedBlackNode(None, "B")
        node.right = RedBlackNode(None, "B")
        node.left.parent = node
        node.right.parent = node
 
    #==========================================================
    # ALL OF INSERT ADJUST
    #==========================================================
     
    # Node and its parent may have a red-red collision. We must adjust.
    def __adjust_insert(self, node):
        if node.parent.color == "B" or node.color == "B": # no red-red collisions
            return
         
        # All root cases
        if (self.root.left == node.parent and self.root.right.color == "R") or (self.root.right == node.parent and self.root.left.color == "R"):
            self.root.left.color = "B"
            self.root.right.color = "B"
            return
         
        # General Red Uncle Case
        if (node.parent.parent.left == node.parent and node.parent.parent.right.color == "R") or (node.parent.parent.right == node.parent and node.parent.parent.left.color == "R"):
            node.parent.parent.left.color = "B"
            node.parent.parent.right.color = "B"
            node.parent.parent.color = "R"
            self.__adjust_insert(node.parent.parent)
            return
         
        # General black uncle case 1
        if (node.parent.parent.left == node.parent and node.parent.parent.right.color == "B" and node.parent.left == node):
            self.__rotate("right",node.parent.parent)
            node.parent.color = "B"
            node.parent.right.color = "R"
            return
         
        # General black uncle case 2
        elif (node.parent.parent.right == node.parent and node.parent.parent.left.color == "B" and node.parent.right == node):
            self.__rotate("left",node.parent.parent)
            node.parent.color = "B"
            node.parent.left.color = "R"
            return
         
        # General black uncle case 3
        elif (node.parent.parent.left == node.parent and node.parent.parent.right.color == "B" and node.parent.right == node):
            self.__rotate("left", node.parent)
            self.__rotate("right", node.parent)
            node.color = "B"
            node.right.color = "R"
            return
             
        # General black uncle case 4
        elif (node.parent.parent.right == node.parent and node.parent.parent.left.color == "B" and node.parent.left == node):
            self.__rotate("right", node.parent)
            self.__rotate("left", node.parent)
            node.color = "B"
            node.left.color = "R"
            return

    # *******************
    # Deletion
    # *******************
 
 
    # Delete a node from the tree.
    # Assumes the node is a regular node in the tree.
    # (Do not use this to delete empty nodes.)
    # Does not return anything.
    
    
    def delete(self, deleted_node):
        parent_child_type = None # Values are None, "left" or "right"
        if deleted_node != self.root:
            parent_child_type = self.__get_parent_type(deleted_node)
  
        # Case zero: deleted_node is empty
  
        if deleted_node.key == None:
            return
               
        # Case one: deleted_node has no children.
        if (deleted_node.left.key == None) and (deleted_node.right.key == None):
            if deleted_node != self.root and deleted_node.parent.left == deleted_node:
                x = RedBlackNode(None,"B")
                deleted_node.parent.left= x
                x.parent = deleted_node.parent
                if deleted_node.color == "R":
                    return
                else:
                    self.__adjust_delete(x)
                return
               
            elif deleted_node != self.root and deleted_node.parent.right == deleted_node:
                x = RedBlackNode(None,"B")
                deleted_node.parent.right = x
                x.parent = deleted_node.parent
                if deleted_node.color == "R":
                    return
                else:
                    self.__adjust_delete(x)
                return
            else:
                self.root = None
                return
 
        # Case two: deleted node has one child
 
        # Subcase 1: deleted node has only left child
 
        elif deleted_node.right.key == None and deleted_node == self.root:
            self.root = deleted_node.left
            self.root.color = "B"
            return
             
        elif deleted_node.right.key == None and deleted_node.parent.left == deleted_node:
            deleted_node.parent.left = deleted_node.left
            deleted_node.left.parent = deleted_node.parent
            deleted_node.left.color = "B"
            return
 
        elif deleted_node.right.key == None and deleted_node.parent.right == deleted_node:
            deleted_node.parent.right = deleted_node.left
            deleted_node.left.parent = deleted_node.parent
            deleted_node.left.color = "B"
            return
 
        # Subcase 2: deleted node has only right child
         
        elif deleted_node.left.key == None and deleted_node == self.root:
            self.root = deleted_node.right
            self.root.color = "B"
            return
             
        elif deleted_node.left.key == None and deleted_node.parent.left == deleted_node:
            deleted_node.parent.left = deleted_node.right
            deleted_node.right.parent = deleted_node.parent
            deleted_node.right.color = "B"
            return
 
        elif deleted_node.left.key == None and deleted_node.parent.right == deleted_node:
            deleted_node.parent.right = deleted_node.right
            deleted_node.right.parent = deleted_node.parent
            deleted_node.right.color = "B"
            return
 
        # Case 3: deleted node has two children
 
        else:
            successor = deleted_node.right
            while successor.left.key != None:
                successor = successor.left
            deleted_node.key = successor.key
            self.delete(successor)
            return
         
    def __adjust_delete(self, node):
        # Case 0: node is root
         
        if node == self.root:
            node.color = "B"
            return
         
        x = self.__get_parent_type(node)
         
        # Case 1: Black S, Black SR, Black SL
         
        if x == "left" and node.parent.right.color == node.parent.right.right.color == node.parent.right.left.color == "B":
            node.parent.right.color = "R"
            if node.parent.color == "B":
                self.__adjust_delete(node.parent)
            else:
                node.parent.color = "B"
            return
 
        if x == "right" and node.parent.left.color == node.parent.left.right.color == node.parent.left.left.color == "B":
            node.parent.left.color = "R"
            if node.parent.color == "B":
                self.__adjust_delete(node.parent)
            else:
                node.parent.color = "B"
            return
 
        # Case 2: Black S, Black Outside Nephew, Red Inside Nephew
 
        if x == "left" and node.parent.right.color == "B" and node.parent.right.left.color == "R" and node.parent.right.right.color == "B":
            self.__rotate("right", node.parent.right)
            node.parent.right.color = "B"
            node.parent.right.right.color = "R"
            self.__adjust_delete(node)
            return
 
        if x == "right" and node.parent.left.color == "B" and node.parent.left.right.color == "R" and node.parent.left.left.color == "B":
            self.__rotate("left", node.parent.left)
            node.parent.left.color = "B"
            node.parent.left.left.color = "R"
            self.__adjust_delete(node)
            return
 
        # Case 3: Black S, Red Outside Nephew
 
        if x == "left" and node.parent.right.color == "B" and node.parent.right.right.color == "R":
            node.parent.right.color = node.parent.color
            self.__rotate("left", node.parent)
            node.parent.parent.right.color = "B"
            node.parent.color = "B"
            return
 
        if x == "right" and node.parent.left.color == "B" and node.parent.left.left.color == "R":
            node.parent.left.color = node.parent.color
            self.__rotate("right", node.parent)
            node.parent.parent.left.color = "B"
            node.parent.color = "B"
            return
 
        # Case 4: Red S
 
        if x == "left" and node.parent.right.color == "R":
            self.__rotate("left",node.parent)
            node.parent.color = "R"
            node.parent.parent.color = "B"
            self.__adjust_delete(node)
            return
         
        if x == "right" and node.parent.left.color == "R":
            self.__rotate("right", node.parent)
            node.parent.parent.color = "B"
            node.parent.color = "R"
            self.__adjust_delete(node)
            return
    
    # **************************************
    # Helper functions (including rotations)
    # **************************************
        
    
    def __get_parent_type(self, node):
        if node == None: return None
        if not node.parent: return None
        return "left" if (node == node.parent.left) else "right"
 
    def __rotate(self, d, old):
        if old.key == None:
            return
         
        if d == "left":
            new = old.right
            if new.key == None:
                return
            sib = new.left
            if old == self.root:
                self.root = new
            elif self.__get_parent_type(old)=="left":
                old.parent.left = new
                new.parent = old.parent
            else:
                old.parent.right = new
                new.parent = old.parent
            new.left = old
            old.parent = new
            old.right = sib
            sib.parent = old
            return
         
        else:
            new = old.left
            if new.key == None:
                return
            sib = new.right
            if old == self.root:
                self.root = new
            elif self.__get_parent_type(old)=="left":
                old.parent.left = new
                new.parent = old.parent
            else:
                old.parent.right = new
                new.parent = old.parent
            new.right = old
            old.parent = new
            old.left = sib
            sib.parent = old
            return
 
    # ***********************************
    # Code that tests whether the tree is red-black.
    # ***********************************
 
    def __is_balanced(self, node = 0):
        if node == 0:
            node = self.root
        if self.get_height(node) == False:
            return False
        else:
            return True
 
    def get_height(self, node = 0):
        if node == 0:
            node = self.root
        if node.key == None:
            return 1
        elif self.get_height(node.left) == False or self.get_height(node.right) == False:
            return False
        elif self.get_height(node.left) == self.get_height(node.right):
            if node.color == "B":
                return self.get_height(node.left) + 1
            else:
                return self.get_height(node.left)
        else:
            return False
 
    def is_red_black(self, node = 0):
        if node == 0:
            node = self.root
        if node == None:
            return True
        if node == self.root and node.left.key == None and node.right.key == None:
            return True
        if node == self.root:
            if self.__is_balanced() == False or node.color== "R":
                return False
        if node.left == None and node.right == None:
            return True
        elif node.left == None or node.right == None:
            return False
        if self.is_red_black(node.right) and self.is_red_black(node.left):
            if node.color == "R" and (node.left.color=="R" or node.right.color=="R"):
                return False
            else:
                return True
        else:
            return False
        

def test():
    num = 27    # Try with higher numbers too!
    inputs = list(range(num))
 
    s = RedBlackTree()
    for input in inputs:
        s.insert(input)
        assert(s.is_red_black())
 
    random.shuffle(inputs)
    for input in inputs:
        s.delete(s.find(input))
        assert(s.is_red_black())
    assert(s.root is None)

def test2():
    num = 22
    # Try with higher numbers too!
    inputs = list(range(num))
    q = RedBlackTree()
    for input in inputs:
        q.insert(input)
        assert(q.is_red_black())
    pretty_print(q)
    random.shuffle(inputs)
    print(inputs)
    assert(q.is_red_black())
    for input in inputs:
        q.delete(q.find(input))
        '''
        pretty_print(q)
        '''
        assert(q.is_red_black())

if __name__ == '__main__':
    for i in range(100):
        test2()


