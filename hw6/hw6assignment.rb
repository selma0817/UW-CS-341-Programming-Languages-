# University of Washington, Programming Languages, Homework 6, hw6runner.rb

# This is the only file you turn in, so do not modify the other files as
# part of your solution.

class MyPiece < Piece
  # The constant All_My_Pieces should be declared here

  # your enhancements here
  # now i have 10 piecs
  # since i don't need to edit the original constructor, 
  # i don't need to use super() here. i want to add 3 more pieces 
  # to the local variable called All_My_Pieces in Piece class 

  # [[0, 0], [1, 0], [0, 1], [1, 1], [1, 2]] # rotate 4 dirs
  # [[0, 0], [0, -1], [0, 1], [0, 2], [0, 3]] # rotate 4 dirs
  # [[0, 0], [1, 0], [1, 1]]
  
  # rotations([[0, 0], [-1, 0], [1, 0], [0, -1], [-1,-1]]), # utah
  All_My_Pieces = [rotations([[0, 0], [0, 1], [0, -1], [-1, 0], [-1, -1]]), 
  [[[0, -2], [0, -1], [0, 0], [0, 1], [0, 2]], 
  [[-2, 0], [-1, 0], [0, 0], [1, 0], [2, 0]]],
  rotations([[0, 0], [1, 0], [1, 1]])] + All_Pieces

  # edit choose next piece so we include newly added 
  def self.next_piece(board)
    MyPiece.new(All_My_Pieces.sample, board)
  end

  Cheat_Piece = [[[0, 0]]]

  def self.next_cheat_piece(board)
    MyPiece.new(Cheat_Piece, board)
  end


end

class MyBoard < Board
  # your enhancements here

  def initialize(game)
    super
    @cheat_flag = false
    @current_block = MyPiece.next_piece(self)
  end

  def cheat
    if score>=100 and !@cheat_flag
      @cheat_flag = true
      @score = score - 100
    end
  end 

  def rotate_180
    if !game_over? and @game.is_running?
      #@current_block.move(0, 0, 2)
      rotate_clockwise
      rotate_clockwise
    end
    draw
  end

  def next_piece
    if @cheat_flag
      @current_block = MyPiece.next_cheat_piece(self)
      @cheat_flag = false
    else 
      @current_block = MyPiece.next_piece(self)
    end  
    @current_pos = nil
  end
  # need to accomodate different size of pieces rather than 4
  # cell size 
  def store_current
    locations = @current_block.current_rotation
    displacement = @current_block.position
    (0..locations.size()-1).each{|index| 
      current = locations[index];
      @grid[current[1]+displacement[1]][current[0]+displacement[0]] = 
      @current_pos[index]
    }
    remove_filled
    @delay = [@delay - 2, 80].max
  end

end


class MyTetris < Tetris
  # your enhancements here

  # need to use MyBoard
  def set_board
    @canvas = TetrisCanvas.new
    @board = MyBoard.new(self)
    @canvas.place(@board.block_size * @board.num_rows + 3,
                  @board.block_size * @board.num_columns + 6, 24, 80)
    @board.draw
  end

  def key_bindings
    super
    @root.bind('u', proc {@board.rotate_180})
    @root.bind('c', proc {@board.cheat})
  end
end

