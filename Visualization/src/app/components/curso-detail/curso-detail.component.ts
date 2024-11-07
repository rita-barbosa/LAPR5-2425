import { Component, Input, OnInit } from '@angular/core'; 
import { Session } from '../../domain/session';

@Component({
 selector: 'curso-detail-angular',
 standalone: true,
 templateUrl: './curso-detail.component.html',
 styleUrls: ['./curso-detail.component.css']
 })

 export class CursoDetailComponent implements OnInit {
  @Input() session? : Session; 
  
  constructor() { }

  ngOnInit(): void {
  }

 }
