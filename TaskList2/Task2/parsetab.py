
# parsetab.py
# This file is automatically generated. Do not edit.
# pylint: disable=W,C,R
_tabversion = '3.10'

_lr_method = 'LALR'

_lr_signature = 'COMMENT NAMEEXP SMTHELSE TAG WRONGCOMMENTexpression : TAGexpression : WRONGCOMMENTexpression : NAMEEXPexpression : COMMENTexpression : SMTHELSE\n    expression : expression expression\n    '
    
_lr_action_items = {'TAG':([0,1,2,3,4,5,6,7,],[2,2,-1,-2,-3,-4,-5,2,]),'WRONGCOMMENT':([0,1,2,3,4,5,6,7,],[3,3,-1,-2,-3,-4,-5,3,]),'NAMEEXP':([0,1,2,3,4,5,6,7,],[4,4,-1,-2,-3,-4,-5,4,]),'COMMENT':([0,1,2,3,4,5,6,7,],[5,5,-1,-2,-3,-4,-5,5,]),'SMTHELSE':([0,1,2,3,4,5,6,7,],[6,6,-1,-2,-3,-4,-5,6,]),'$end':([1,2,3,4,5,6,7,],[0,-1,-2,-3,-4,-5,-6,]),}

_lr_action = {}
for _k, _v in _lr_action_items.items():
   for _x,_y in zip(_v[0],_v[1]):
      if not _x in _lr_action:  _lr_action[_x] = {}
      _lr_action[_x][_k] = _y
del _lr_action_items

_lr_goto_items = {'expression':([0,1,7,],[1,7,7,]),}

_lr_goto = {}
for _k, _v in _lr_goto_items.items():
   for _x, _y in zip(_v[0], _v[1]):
       if not _x in _lr_goto: _lr_goto[_x] = {}
       _lr_goto[_x][_k] = _y
del _lr_goto_items
_lr_productions = [
  ("S' -> expression","S'",1,None,None,None),
  ('expression -> TAG','expression',1,'p_expression_tag','main.py',48),
  ('expression -> WRONGCOMMENT','expression',1,'p_expression_wrongcomment','main.py',52),
  ('expression -> NAMEEXP','expression',1,'p_expression_nameexp','main.py',57),
  ('expression -> COMMENT','expression',1,'p_expression_comment','main.py',61),
  ('expression -> SMTHELSE','expression',1,'p_expression_SMTHELSE','main.py',65),
  ('expression -> expression expression','expression',2,'p_expression','main.py',71),
]
