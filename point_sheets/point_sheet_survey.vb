Private Sub CommandButton1_Click()

'define variables:
    qa_user = Sheets("pointsheet").Range("E7")
    qa_time = Sheets("pointsheet").Range("E8")
    qa_student = Sheets("pointsheet").Range("E9")
    qa_period = Sheets("pointsheet").Range("E10")
    qa_respect = Sheets("pointsheet").Range("E12")
    qa_ontask = Sheets("pointsheet").Range("E13")
    qa_focus = Sheets("pointsheet").Range("E14")
    qa_focnot = Sheets("pointsheet").Range("E15")
    qa_notes = Sheets("pointsheet").Range("E16")
    
'define required questions:
    If qa_user = "" Then
        MsgBox "Please provide your surname"
        Exit Sub
    End If
    
    If qa_student = "" Then
        MsgBox "Please provide The Student's Name"
        Exit Sub
    End If
    
    If qa_period = "" Then
        MsgBox "Please provide the class period"
        Exit Sub
    End If
    
    If qa_respect = "" Then
        MsgBox "Please provide points for Respect"
        Exit Sub
    End If
    
    If qa_ontask = "" Then
        MsgBox "Please provide points for Time on Task"
        Exit Sub
    End If
    
    If qa_focus = "" Then
        MsgBox "Please provide points for Focus Goal"
        Exit Sub
    End If
    
'Add Data to data spreadsheet
    row_number = 1
    Do
    DoEvents
    row_number = row_number + 1
        item_in_review = Sheets("data").Range("A" & row_number)
    
    Loop Until item_in_review = ""
    
    last_transaction_id = Sheets("data").Range("A" & (row_number - 1))
    
    Dim next_transaction_id As Integer
    next_transaction_id = last_transaction_id + 1
    
    ' What goes Where?
    'trans id
    Sheets("data").Range("A" & (row_number)) = next_transaction_id
    'user
    Sheets("data").Range("B" & (row_number)) = qa_user
    'time
    Sheets("data").Range("C" & (row_number)) = qa_time
    'student
    Sheets("data").Range("D" & (row_number)) = next_transaction_id
    'period
    Sheets("data").Range("E" & (row_number)) = qa_period
    'respect
    Sheets("data").Range("F" & (row_number)) = qa_respect
    'on_time
    Sheets("data").Range("G" & (row_number)) = qa_ontask
    'focus
    Sheets("data").Range("H" & (row_number)) = qa_focus
    'focus notes
    Sheets("data").Range("I" & (row_number)) = qa_focnot
    'other notes
    Sheets("data").Range("J" & (row_number)) = qa_notes





End Sub
